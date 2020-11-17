-module(a).

-export([run/0]).

run() ->
    read("small_input.txt").

read(Filename) ->
    {ok, File} = file:open(Filename, read),
    read(File, []).

read(File, Acc) ->
    case io:get_line(File, '') of
        eof ->
            lists:reverse(Acc);
        Line ->
            read(File, [parse(Line) | Acc])
    end.

parse(Line) ->
    case string:tokens(Line, " \n") of
        [Command, Register, Value] ->
            {list_to_atom(Command), list_to_atom(Register), parse_right_op(Value)};
        [Command, Register] ->
            {list_to_atom(Command), list_to_atom(Register)}
    end.

parse_right_op(RightOperand) ->
    try
        list_to_integer(RightOperand)
    catch error:badarg ->
        list_to_atom(RightOperand)
    end.
