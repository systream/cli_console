-module(prop_parser).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL({Commands, Flags}, {cli_commands(), flags()},
        begin
            Input = Commands ++ " " ++ Flags,
            {ok, ParsedCommand, ParsedFlags} = cli_console_parser:parse(Input),

            NonTrueValues = [Value ||{_, Value} <- ParsedFlags, Value =/= true],

            check_contains(ParsedCommand, Input) andalso
            check_contains(proplists:get_keys(ParsedFlags), Input) andalso
            check_contains(NonTrueValues, Input)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
check_contains([], _) ->
  true;
check_contains([Command | RestOdCommands], Input) ->
  case string:find(Input, Command) of
    nomatch ->
      false;
    _ ->
      check_contains(RestOdCommands, Input)
  end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

cli_commands() ->
  cli_commands([], rand:uniform(16)).

cli_commands(StaringFragments, 0) ->
  ?LET(Final, StaringFragments, begin lists:flatten(Final) end);
cli_commands(StringFragments, Count) ->
  cli_commands([cli_command(), space() | StringFragments], Count-1).

cli_command() ->
  console_data().

flags() ->
    flags([], rand:uniform(150)).

flags(StaringFragments, 0) ->
    ?LET(Final, StaringFragments, begin lists:flatten(Final) end);
flags(StringFragments, Count) ->
    flags([flag_generator(), space() | StringFragments], Count-1).

flag_generator() ->
    ?LET({FlagMarker, FlagKey, FlagSeparator, FlagValue},
         {flag_marker(), flag_key(), flag_separator_value(), flag_value()},
          begin
              Flag = FlagMarker ++ FlagKey,
              case FlagValue of
                  no_value ->
                      Flag;
                  _ ->
                    Flag ++ FlagSeparator ++ FlagValue
              end
          end).

console_data() ->
  non_empty(list(frequency([{80, range($a, $z)},              % letters
                  {60, range($A, $Z)},              % letters
                  {1,  $\n},                        % linebreak
                  {1, oneof([$., $-, $!, $?, $,])}, % punctuation
                  {1, range($0, $9)}                % numbers
                 ]))).

flag_key() ->
    frequency([{5, console_data()}, {1, "help"}]).

flag_marker() ->
    oneof(["-", "--"]).

flag_value() ->
    frequency([
                {3, console_data()},
                {2, no_value}
              ]).

flag_separator_value() ->
    oneof([" ", "=", random_space()]).

space() ->
    frequency([{10, " "},
               {3, random_space()},
               {1, $\\}
              ]).

random_space() ->
  non_empty(list($\s)).

