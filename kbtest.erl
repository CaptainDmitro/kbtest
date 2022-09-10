-module(kbtest).

-export([current_state/0, count_codelines/1]).

-export([start/0, stop/0]).

-define(SERVER, ?MODULE).

-define(LINE_WITH_CODE_PATTERN, "^.*[a-zA-Z0-9]+.*%").
-define(ERL_EXTENSION, ".erl").

%%% Public API

start() ->
    register(?SERVER, spawn(fun () -> loop(0) end)).

stop() ->
    rpc({stop, stopped}).

count_codelines(Dir) ->
    rpc({list_dir, Dir}).

current_state() ->
    rpc(current_state).

%%%

rpc(Request) ->
    ?SERVER ! {self(), Request},
    receive
        {_Pid, Response} ->
            Response
    end.

loop(State) ->
    receive
        {From, {list_dir, Dir}} ->
            From ! {self(), {noreply, list_dir}},
            spawn_link(fun() -> list_files(Dir) end),
            loop(State);
        {_From, {update, _File, Count}} ->
            loop(State+Count);
        {_From, reset_state} ->
            loop(0);
        {From, current_state} ->
            From ! {self(), {current_state, State}},
            loop(State);
        {From, {stop, Reason}} ->
            From ! {self(), {exit, Reason}},
            exit({exit, Reason})
    end.

list_files(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ?SERVER ! {self(), reset_state},
            {ok, Files} = file:list_dir(Dir),
            FilePaths = [filename:join(Dir, FileName) || FileName <- Files], % join filename and basename e.g. basename is "." and filename is "t.erl" then fullname is "./t.erl"
            spawn_link(fun() -> list_files(FilePaths, []) end);
        false ->
            throw({error, Dir, not_a_dir}) % throw an error if given 'Dir' is not a directory
    end.

list_files([H | T], Acc) ->
    list_files(T,
        case filelib:is_dir(H) of
            true ->
                {ok, List} = file:list_dir(H),
                FileNames = [filename:join(H, FileName) || FileName <- List],
                spawn_link(fun() -> list_files(FileNames, Acc) end); % if dir is found then process it in a parallel process
            false -> case filename:extension(H) =:= ?ERL_EXTENSION of
                true ->
                    [spawn_link(fun() -> read_file(H) end) | Acc]; % if .erl file found then process it in a parallel process
                false -> Acc
            end
        end
    );
list_files([], Acc) -> Acc.

read_file(File) ->
    {ok, Device} = file:open(File, [read]),
    try read_line(Device, 0) of
        Result ->
            ?SERVER ! {self(), {update, File, Result}}
        after
            file:close(Device)
    end.

read_line(Device, Acc) ->
    case file:read_line(Device) of
        eof ->  Acc;
        {ok, Line} ->
            case re:run(Line, ?LINE_WITH_CODE_PATTERN) of
                {match, _} -> read_line(Device, Acc+1);
                nomatch -> read_line(Device, Acc)
            end
    end.
