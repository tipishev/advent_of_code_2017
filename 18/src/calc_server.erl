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
            Regs = maps:new(),
            Result = run(Regs, Commands, _Current=1, undefined),
            Client ! {Ref, Result},
            loop(State);
        {Client, Ref, shutdown} ->
            Client ! {Ref, ok},
            exit(normal);
        Any ->
            io:format("Received unexpected: ~p~n", [Any]),
            loop(State)
    end.

run(_, Commands, Current, _) when Current > length(Commands)-> out_of_bounds;
run(_, _, Current, _) when Current < 0 -> out_of_bounds;
run(Regs, Commands, Current, LastSnd) ->

    Command = lists:nth(Current, Commands),

    % shortcut functions
    Resolve = fun(Arg) -> resolve(Arg,  Regs) end,
    Get = fun(Reg) -> maps:get(Reg, Regs, 0) end,
    Set = fun(Reg, Value) -> Regs#{Reg => Value} end,
    Advance = fun(NewRegs) -> run(NewRegs, Commands, Current + 1, LastSnd) end,
    Nop = fun() -> Advance(Regs) end,

    case Command of

        {set, {reg, Reg}, Arg} ->
            Advance(Set(Reg, Resolve(Arg)));

        {add, {reg, Reg}, Arg} ->
            Advance(Set(Reg, Get(Reg) + Resolve(Arg)));

        {mul, {reg, Reg}, Arg} ->
            Advance(Set(Reg, Get(Reg) * Resolve(Arg)));

        {mod, {reg, Reg}, Arg} ->
            Advance(Set(Reg, Get(Reg) rem Resolve(Arg)));

        {snd, {reg, Reg}} ->
            run(Regs, Commands, Current + 1, _LastSend=Get(Reg));

        {rcv, {reg, Reg}} ->
            case get(Reg) of
                0 -> Nop();
                _ -> LastSnd  % the money shot
            end;

        {jgz, {reg, Reg}, Arg}->
            case Get(Reg) > 0 of
                true ->
                    run(Regs, Commands, Current +  Resolve(Arg), LastSnd);
                false ->
                    Nop()
            end;

        Any -> 
            {unknown, Any, regs, Regs}
    end.

resolve({int, Int}, _Regs) -> Int;
resolve({reg, Reg}, Regs) ->
    maps:get(Reg, Regs, 0).
