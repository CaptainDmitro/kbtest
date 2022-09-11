-module(kbtest).

-export([current_state/0, count_codelines/1]).

-export([start/0, stop/0]).

-define(SERVER, ?MODULE).

-define(LINE_WITH_CODE_PATTERN, "^.*[a-zA-Z0-9]+.*%").
-define(ERL_EXTENSION, ".erl").

-record(state, {code_line_count=0}).

%%% Public API

start() ->
    register(?SERVER, spawn(fun () -> loop(#state{}) end)).

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
            self() ! {self(), reset_state},
            spawn_link(fun() -> list_files(Dir) end),
            loop(State);
        {_From, {update, _File, Count}} ->
            loop(State#state{code_line_count=State#state.code_line_count+Count});
        {_From, reset_state} ->
            loop(#state{});
        {From, current_state} ->
            From ! {self(), {code_lines_found, State}},
            loop(State);
        {From, {stop, Reason}} ->
            From ! {self(), {exit, Reason}},
            exit({exit, Reason})
    end.

list_files(Path) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, Files} = file:list_dir(Path),
            FilePaths = [filename:join(Path, FileName) || FileName <- Files],
            [spawn_link(fun() -> list_files(NewPath) end) || NewPath <- FilePaths]; % create a new process for every file in given directory
        false ->
            case filelib:is_file(Path) of
                true ->
                    case filename:extension(Path) =:= ?ERL_EXTENSION of
                        true ->
                            spawn_link(fun() -> read_file(Path) end); % create a new process for every .erl file
                        false -> not_erl_file
                    end;
                false ->
                    error({not_a_dir, Path})
            end
    end.

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
