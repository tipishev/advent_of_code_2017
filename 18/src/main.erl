-module(main).

% Public API
-export([run/0]).

%%% Public API
run() ->
    InputServer = input_server:start_link(),
    % Filename =  "../inputs/small_input.txt",
    Filename =  "../inputs/input.txt",
    Commands = input_server:read_commands(InputServer, Filename),

    CalcServer = calc_server:start_link(),
    calc_server:run_commands(CalcServer, Commands).
