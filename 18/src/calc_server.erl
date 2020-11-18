-module(calc_server).

%% public API
-export([start_link/0, run_commands/2, shutdown/1]).

%% internals
-export([init/1]).

-record(state, {client}).

%%% Public API

start_link() ->
    spawn_link(?MODULE, init, [self()]).

run_commands(Pid, Commands) ->
    rpc(Pid, {run, Commands}).

shutdown(Pid) ->
    rpc(Pid, shutdown).

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
        {Client, Ref, {run, Commands}} ->
            Registers = maps:new(),
            Result = run(Registers, Commands, _Current=1, undefined),
            Client ! {Ref, Result},
            loop(State);
        {Client, Ref, shutdown} ->
            Client ! {Ref, ok},
            exit(normal);
        Any ->
            io:format("Received unexpected: ~p~n", [Any]),
            loop(State)
    end.


run(Registers, Commands, Current, LastSnd) ->
    Command = lists:nth(Current, Commands),
    case Command of

        {set, {reg, Reg}, Arg} ->
            NewRegisters = Registers#{Reg => resolve(Arg, Registers)},
            run(NewRegisters, Commands, Current + 1, LastSnd);

        {add, {reg, Reg}, Arg} ->
            OldValue = maps:get(Reg, Registers, 0),
            NewValue = OldValue + resolve(Arg, Registers),
            run(Registers#{Reg => NewValue}, Commands, Current + 1, LastSnd);

        {mul, {reg, Reg}, Arg} ->
            OldValue = maps:get(Reg, Registers, 0),
            NewValue = OldValue * resolve(Arg, Registers),
            run(Registers#{Reg => NewValue}, Commands, Current + 1, LastSnd);

        {mod, {reg, Reg}, Arg} ->
            OldValue = maps:get(Reg, Registers, 0),
            NewValue = OldValue rem resolve(Arg, Registers),
            run(Registers#{Reg => NewValue}, Commands, Current + 1, LastSnd);

        {snd, {reg, Reg}} ->
            Snd = maps:get(Reg, Registers, 0),
            run(Registers, Commands, Current + 1, Snd);

        {rcv, {reg, Reg}} ->
            case maps:get(Reg, Registers, 0) of
                0 -> run(Registers, Commands, Current + 1, LastSnd);  % NOP
                _ -> LastSnd  % the money shot
            end;

        {jgz, {reg, Reg}, Arg} ->
            RegValue = maps:get(Reg, Registers, 0),
            case RegValue > 0 of
                true ->
                    Offset = resolve(Arg, Registers),
                    run(Registers, Commands, Current + Offset, LastSnd);
                false ->
                    run(Registers, Commands, Current + 1, LastSnd)
            end;

        Any -> 
            {unknown, Any, regs, Registers}
    end.

resolve({int, Int}, _Registers) -> Int;
resolve({reg, Reg}, Registers) ->
    maps:get(Reg, Registers, 0).
