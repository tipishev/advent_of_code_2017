-module(input_server).

%% public API
-export([start_link/0]).

%% internals
-export([init/1, read_commands/2, shutdown/1]).

-record(state, {client}).

%%% Public API

start_link() ->
    spawn_link(?MODULE, init, [self()]).

read_commands(Pid, Filename) ->
    rpc(Pid, {read, Filename}).

shutdown(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, shutdown},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    % after 1000 -> timeout  % unnecessary
    end.

% passing Pid is annoying. Using OTP should fix that.
rpc(Pid, Message) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Message},
    receive
        {Ref, Response} -> Response
    after 1000 ->
          timeout
    end.


%%% Internals

init(Client) ->
    loop(#state{client=Client}).

loop(State = #state{client=Client}) ->
    receive
        {Client, Ref, {read, Filename}} ->
            Commands = read(Filename),
            Client ! {Ref, Commands},
            loop(State);
        {Client, Ref, shutdown} ->
            Client ! {Ref, ok},
            exit(normal);
        Any ->
            io:format("Received unexpected: ~p~n", [Any]),
            loop(State)
    end.

read(Filename) ->
    case file:open(Filename, read) of
        {ok, File} ->
            read(File, []);
        {error, Reason} ->
            {error, Reason}
    end.

read(File, Acc) ->
    case io:get_line(File, '') of
        eof ->
            lists:reverse(Acc);
        Line ->
            read(File, [parse(Line) | Acc])
    end.

parse(Line) ->
    case string:tokens(Line, " \n") of
        [Command, Register, IntOrReg] ->
            {list_to_atom(Command), {reg, list_to_atom(Register)},
             parse_int_or_reg(IntOrReg)};
        [Command, IntOrReg] ->
            {list_to_atom(Command), parse_int_or_reg(IntOrReg)}
    end.

parse_int_or_reg(IntOrReg) ->
    try
        {int, list_to_integer(IntOrReg)}
    catch error:badarg ->
        {reg, list_to_atom(IntOrReg)}
    end.
