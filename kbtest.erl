-module(kbtest).

-export([start/1, stop/0]).

-export([current_state/0]).

current_state() ->
    rpc(current_state).

start(Dir) ->
    register(?MODULE, spawn(fun () -> loop(0) end)),
    list_dir(Dir).

list_dir(Dir) ->
    rpc({list_dir, Dir}).

stop() ->
    rpc(stop).

rpc(Request) ->
    ?MODULE ! {self(), Request},
    receive
        {_Pid, Response} ->
            Response
    end.

loop(State) ->
    receive
        {From, {list_dir, Dir}} ->
            From ! {self(), {noreply, list_dir, ok}},
            spawn_link(fun() -> list_files(Dir) end),
            loop(State);
        {_From, {update, _File, Count}} ->
            loop(State+Count);
        {From, current_state} ->
            From ! {self(), {current_state, State}},
            loop(State);
        {From, stop} ->
            From ! {self, {stopped, State}},
            exit(stopped)
    end.

list_files(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            spawn_link(fun() -> list_files(Files, []) end);
        false ->
            {error, not_a_dir}
    end.

list_files([H | T], Acc) ->
    list_files(T,
        case filelib:is_dir(H) of
            true ->
                {ok, List} = file:list_dir(H),
                FileNames = [filename:join(H, FileName) || FileName <- List],
                spawn_link(fun() -> list_files(FileNames, Acc) end);
            false -> case filename:extension(H) =:= ".erl" of
                true -> [spawn_link(fun() -> read_file(H) end) | Acc];
                false -> Acc
            end
        end
    );
list_files([], Acc) -> Acc.

read_file(File) ->
    {ok, Device} = file:open(File, [read]),
    try read_line(Device, 0) of
        Result ->
            ?MODULE ! {self(), {update, File, Result}}
        after
            file:close(Device)
    end.

read_line(Device, Acc) ->
    case file:read_line(Device) of
        eof ->  Acc;
        {ok, Line} ->
            case re:run(Line, "^.*[a-zA-Z0-9]+.*%") of
                {match, _} -> read_line(Device, Acc+1);
                nomatch -> read_line(Device, Acc)
            end
    end.
