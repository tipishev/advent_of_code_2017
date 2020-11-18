-module(main).

% Public API
-export([a/0, b/0]).

%%% Public API
a() ->
    InputServer = input_server:start_link(),
    % Filename =  "../inputs/small_input.txt",
    Filename =  "../inputs/input.txt",
    Commands = input_server:read_commands(InputServer, Filename),

    CalcServer = calc_server:start_link(),
    calc_server:run_commands(CalcServer, Commands).

b() ->
    InputServer = input_server:start_link(),
    Filename =  "../inputs/small_input.txt",
    % Filename =  "../inputs/input.txt",
    Commands = input_server:read_commands(InputServer, Filename),

    CalcServer0 = calc_server2:start_link(0),
    CalcServer1 = calc_server2:start_link(1),

    calc_server2:pair(CalcServer0, CalcServer1),
    calc_server2:pair(CalcServer1, CalcServer0),

    calc_server2:run_commands(CalcServer0, Commands),
    calc_server2:run_commands(CalcServer1, Commands).

    % calc_server2:count_sends(CalcServer1).
