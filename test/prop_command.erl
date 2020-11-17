-module(prop_command).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
  application:ensure_all_started(cli_console),
    ?FORALL({ArgsDef, Args, Description}, {arguments_defs(), arguemnts(), string()},
            begin
              Command = ["cmd", "run"],
              cli_console_command:register(Command, ArgsDef, fun(A) -> A end, Description),
              case cli_console_command:run(Command, Args) of
                {ok, _ArgsResult} ->
                  %@TODO: better evaluation
                  true;
                {error, _} ->
                  %@TODO: better evaluation
                  true
              end
            end).

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
    ?LET({Key, Type, IsMandatory}, {key(), argument_type(), is_mandatory()},
         begin
            Arg = cli_console_command_arg:argument(Key, Type),
            case IsMandatory of
                true ->
                    cli_console_command_arg:mandatory(Arg);
                _ ->
                    Arg
            end
        end).

is_mandatory() ->
    frequency([{10, false}, {1, true}]).

argument_type() ->
    oneof([flag, atom, string, binary, integer]).

key() ->
  Chars = [ [Char] || Char <- lists:seq($a, $z)],
  oneof(["foo", "bar", "node" | Chars]).

value() ->
    non_empty(list(frequency([{80, range($a, $z)},              % letters
                              {60, range($A, $Z)},              % letters
                              {1,  $\n},                        % linebreak
                              {1, oneof([$., $-, $!, $?, $,])}, % punctuation
                              {1, range($0, $9)}                % numbers
                             ]))).


