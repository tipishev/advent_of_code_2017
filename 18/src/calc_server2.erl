-module(calc_server2).

%% public API
-export([start_link/1, pair/2, run_commands/2, shutdown/1]).

%% internals
-export([init/2]).

-record(state, {client, proc_id, partner}).

%%% Public API

start_link(ProcId) ->
    spawn_link(?MODULE, init, [self(), ProcId]).

run_commands(Pid, Commands) ->
    rpc(Pid, {run, Commands}).

pair(Pid, Partner) ->
    rpc(Pid, {pair, Partner}).

shutdown(Pid) ->
    rpc(Pid, shutdown).

% passing Pid is annoying. Using OTP should fix that.
rpc(Pid, Message) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Message},
    receive
        {Ref, Response} -> Response
    after 1000 ->
          rpc_timeout
    end.


%%% Internals

init(Client, ProcId) ->
    loop(#state{client=Client, proc_id=ProcId}).

loop(State = #state{client=Client, proc_id=ProcId}) ->
    receive
        {Client, Ref, {pair, Partner}} ->
            Client ! {Ref, ok},
            loop(State#state{partner=Partner});
        {Client, Ref, {run, Commands}} ->
            Regs = #{p => ProcId},
            Result = run(Regs, Commands, _Current=1, 0, State#state.partner),
            Client ! {Ref, Result},
            loop(State);
        {Client, Ref, shutdown} ->
            Client ! {Ref, ok},
            exit(normal);
        Any ->
            io:format("Received unexpected: ~p~n", [Any]),
            loop(State)
    end.

run(_, Commands, Current, _, _) when Current > length(Commands)-> out_of_bounds;
run(_, _, Current, _, _) when Current < 0 -> out_of_bounds;
run(Regs, Commands, Current, SndCount, Partner) ->

    Command = lists:nth(Current, Commands),

    % shortcut functions
    Resolve = fun(Arg) -> resolve(Arg,  Regs) end,
    Get = fun(Reg) -> maps:get(Reg, Regs, 0) end,
    Set = fun(Reg, Value) -> Regs#{Reg => Value} end,
    Advance = fun(NewRegs) -> run(NewRegs, Commands, Current + 1, SndCount, Partner) end,
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
            Partner ! {self(), Get(Reg)},
            run(Regs, Commands, Current + 1, SndCount + 1, Partner);

        {rcv, {reg, Reg}} ->
            receive
                {Partner, Value} ->
                    Set(Reg, Value)
            after 1000 ->
                {timeout_in_rcv, SndCount}
            end;

        {jgz, {reg, Reg}, Arg}->
            case Get(Reg) > 0 of
                true ->
                    run(Regs, Commands, Current +  Resolve(Arg), SndCount, Partner);
                false ->
                    Nop()
            end;

        Any -> 
            {unknown, Any, regs, Regs}
    end.

resolve({int, Int}, _Regs) -> Int;
resolve({reg, Reg}, Regs) ->
    maps:get(Reg, Regs, 0).
