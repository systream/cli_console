-module(prop_command).
-include_lib("proper/include/proper.hrl").
-include("../src/cli_console_command.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
  application:ensure_all_started(cli_console),
    ?FORALL({Command, ArgsDef, Args, Description},
            {test_command(), arguments_defs(), arguemnts(), string()},
            begin
              %Command = ["cmd", "run"],
              case cli_console_command:register(Command, ArgsDef, fun(A) -> A
                                                                  end,
                                                Description) of
                ok ->

                  % test against crash, or unhandled stuff
                  case cli_console_command:run(normalize_command(Command), Args) of
                    {ok, ArgsResult} ->
                      assert_ok(ArgsDef, ArgsResult, Args);
                    {error, Error} ->
                      assert_error(Error)
                  end;
                {error, _} ->
                  true
              end
            end).

assert_ok(_ArgsDef, [], _Args) ->
  true;
assert_ok(ArgsDef, [{Key, Result} | Rest], Args) ->
  %io:format("Argdef: ~p ~n Argres: ~p~n", [ArgsDef, {Key, Result}]),
  case lists:keyfind(Key, 2, ArgsDef) of
    false ->
      false;
    #argument{type = boolean} when Result == true orelse Result == false ->
      assert_ok(ArgsDef, Rest, Args);
    #argument{type = integer} when is_integer(Result) ->
      assert_ok(ArgsDef, Rest, Args);
    #argument{type = flag} when Result == true orelse Result == false ->
      assert_ok(ArgsDef, Rest, Args);
    #argument{type = atom} when is_atom(Result)->
      assert_ok(ArgsDef, Rest, Args);
    #argument{type = binary} when is_binary(Result)->
      assert_ok(ArgsDef, Rest, Args);
    #argument{type = string} when is_list(Result)->
      assert_ok(ArgsDef, Rest, Args);
    #argument{} = A ->
      io:format("~nUnhandled ok assert: ~p~nValue: ~p~nArgs: ~p~n", [A, Result, Args ]),
      io:format("~n------------------ ~nArgdefs: ~p~n~n", [ArgsDef]),
      %assert_ok(ArgsDef, Rest),
      false
  end;
assert_ok(_ArgsDef, [{format, _, _} | _], Args) ->
  case lists:keyfind("help", 1, Args) of
    undefined ->
      io:format("Unexpected help printed~n"),
      false;
    _ ->
      true
  end.

normalize_command([_, _] = Command) ->
  Command;
normalize_command([A, B, _C])  ->
  [A, B, "1"].

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

arguemnts() ->
  arguemnts([], rand:uniform(32)).

arguemnts(StaringFragments, 0) ->
  ?LET(Final, StaringFragments, begin lists:flatten(Final) end);
arguemnts(StringFragments, Count) ->
  arguemnts([{key(), oneof([value(), true])} | StringFragments], Count-1).

arguments_defs() ->
    arguments_defs([], rand:uniform(32)).

arguments_defs(StaringFragments, 0) ->
    ?LET(Final, StaringFragments, begin lists:flatten(Final) end);
arguments_defs(StringFragments, Count) ->
    arguments_defs([arguments_def() | StringFragments], Count-1).

arguments_def() ->
    ?LET({Key, Type, IsMandatory, Default},
         {key(), argument_type(), is_mandatory(), default()},
         begin
            Arg = cli_console_command_arg:argument(Key, Type, Key ++ " desc"),
            Arg1 = maybe_set_mandatory(IsMandatory, Arg),
            maybe_set_default(Default, Arg1)
        end).

test_command() ->
  frequency([{20, ["cmd", "run"]},
             {1, ["cmd", "run", arguments_def()]}]).

maybe_set_mandatory(true, Arg) ->
  cli_console_command_arg:mandatory(Arg);
maybe_set_mandatory(false, Arg) ->
  Arg.

maybe_set_default(no_default_value_set, Arg) ->
  Arg;
maybe_set_default(Default, Arg) ->
  cli_console_command_arg:set_default(Arg, Default).

is_mandatory() ->
  frequency([{10, false}, {1, true}]).

default() ->
  frequency([
              {10, no_default_value_set},
              {3, frequency([{3, integer()},
                             {10, string()},
                             {1, boolean()}])}
            ]).

argument_type() ->
    oneof([flag, atom, string, binary, integer]).

key() ->
  Chars = [[Char] || Char <- lists:seq($a, $z)],
  frequency([{1, "help"},
             {50, oneof(["foo", "bar", "node", "test", "limit" | Chars])}
            ]).

value() ->
    non_empty(list(frequency([{80, range($a, $z)},              % letters
                              {60, range($A, $Z)},              % letters
                              {1,  $\n},                        % linebreak
                              {1, oneof([$., $-, $!, $?, $,])}, % punctuation
                              {1, range($0, $9)}                % numbers
                             ]))).


assert_error({not_convertible, {_, string, Actual}}) when is_list(Actual) ->
  false;
assert_error({not_convertible, {_, binary, Actual}}) when is_binary(Actual) ->
  false;
assert_error({not_convertible, {_, integer, Actual}}) when is_integer(Actual) ->
  false;
assert_error({not_convertible, {_, boolean, Actual}}) when Actual == true orelse
                                                           Actual == false ->
  false;
assert_error({not_convertible, {_, flag, Actual}}) when Actual =:= true orelse
                                                        Actual =:= false ->
  false;
assert_error({not_convertible, {_, atom, Actual}}) when is_atom(Actual) ->
  false;
assert_error({not_convertible, _}) ->
  true;
assert_error({missing_arguments, []}) ->
  false;
assert_error({missing_arguments, _}) ->
  true;
assert_error(Err) ->
  io:format("Unhandled assert error: ~p~n", [Err]),
  true.