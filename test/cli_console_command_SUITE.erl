%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, Systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_command_SUITE).
-author("Pete Tihanyi").

%% API
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DUMMY_FUN(Ret), fun(_) -> Ret end).
-define(DUMMY_FUN, ?DUMMY_FUN(ok)).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
  [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  application:ensure_all_started(cli_console),
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  application:stop(cli_console),
  ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
  ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  application:ensure_all_started(cli_console),
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
  ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
  [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
  [ register_no_argument,
    register_the_twice,
    register_wildcard,
    register_multiple_wildcard,
    register_wildcard_same_arg,
    register_wildcard_different_args,
    command_not_found,
    mandatory_arg_not_set_found,
    mandatory_arg_not_set_but_has_default,
    get_default_if_arg_not_specified,
    run_with_mixed_args,
    arg_convert_happy,
    handle_cast_for_cover

  ].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
register_no_argument(_Config) ->
  ok = cli_console_command:register(["test"], [], ?DUMMY_FUN, ""),
  ?assertEqual({ok, ok}, cli_console_command:run(["test"], [])).

register_the_twice(_Config) ->
  ok = cli_console_command:register(["foo"], [], ?DUMMY_FUN, ""),
  ok = cli_console_command:register(["foo"], [], ?DUMMY_FUN(ok2), ""),
  ?assertEqual({ok, ok2}, cli_console_command:run(["foo"], [])).

register_wildcard(_Config) ->
  Fun = fun(Args) -> Args end,
  WildcardArgument = cli_console_command_arg:argument("wildcard", string,
                                                      "Desc of wildcard"),
  ok = cli_console_command:register(["wild", WildcardArgument], [], Fun,
                                    "Wild help"),
  ?assertEqual({ok, [{"wildcard", "things"}]},
               cli_console_command:run(["wild", "things"], [])).

register_multiple_wildcard(_Config) ->
  Fun = fun(Args) -> Args end,
  WildcardArgument = cli_console_command_arg:argument("wildcard", string,
                                                      "Desc of wildcard"),
  WildcardArgument2 = cli_console_command_arg:argument("wildcard2", string,
                                                      "Desc of wildcard2"),
  ok = cli_console_command:register(["wild", WildcardArgument, WildcardArgument2],
                                    [], Fun, "Wild help"),
  ?assertEqual({ok, [{"wildcard2", "wild"}, {"wildcard", "things"}]},
               cli_console_command:run(["wild", "things", "wild"], [])).

register_wildcard_same_arg(_Config) ->
  Fun = fun(Args) -> {ok, Args} end,
  WildcardArgument = cli_console_command_arg:argument("wildcard", string,
                                                      "Desc of wildcard"),
  Result = cli_console_command:register(["wild", WildcardArgument],
                                    [WildcardArgument], Fun, "Wild help"),
  ?assertEqual({error, {multiple_argument_definitions, "wildcard"}}, Result).

register_wildcard_different_args(_Config) ->
  WildcardArgument = cli_console_command_arg:argument("wildcard", string,
                                                      "Desc of wildcard"),
  WildcardArgumentDefault =
    cli_console_command_arg:set_default(WildcardArgument, "test"),
  ok = cli_console_command:register(["wild", WildcardArgument], [],
                                    fun(_Args) -> {text, "1"} end,
                                    "Wild help"),

  ok = cli_console_command:register(["wild", WildcardArgumentDefault], [],
                                    fun(_Args) -> {text, "2"} end,
                                    "Wild help"),
  ?assertEqual({ok, {text, "2"}},
               cli_console_command:run(["wild", "things"], [])).

command_not_found(_Config) ->
  ?assertEqual({error, command_not_found},
               cli_console_command:run(["not_found_command"], [])).

mandatory_arg_not_set_found(_Config) ->
  Foo = cli_console_command_arg:argument("foo", string, "foo desc"),
  FooRequired = cli_console_command_arg:mandatory(Foo),

  cli_console_command:register(["test1"], [FooRequired], ?DUMMY_FUN, ""),

  ?assertEqual({error, {missing_arguments, [FooRequired]}},
               cli_console_command:run(["test1"], [])).

mandatory_arg_not_set_but_has_default(_Config) ->
  Foo = cli_console_command_arg:argument("foo", string, "foo desc"),
  FooWithDefault = cli_console_command_arg:set_default(Foo, "test"),
  FooRequired = cli_console_command_arg:mandatory(FooWithDefault),

  cli_console_command:register(["test1"], [FooRequired], ?DUMMY_FUN, ""),

  ?assertEqual({ok, ok}, cli_console_command:run(["test1"], [])).

get_default_if_arg_not_specified(_Config) ->
  Foo = cli_console_command_arg:argument("foo", string, "doo desc"),
  FooWithDefault = cli_console_command_arg:set_default(Foo, "test"),
  FooRequired = cli_console_command_arg:mandatory(FooWithDefault),

  cli_console_command:register(["test1"], [FooRequired], fun(A) -> A end, ""),

  ?assertEqual({ok, [{"foo", "test"}]},
               cli_console_command:run(["test1"], [])).

run_with_mixed_args(_Config) ->
  Simple = cli_console_command_arg:argument("simple", string, "foo desc"),
  SimpleWithDefault = cli_console_command_arg:set_default(Simple, "test"),
  Mandatory = cli_console_command_arg:mandatory(
    cli_console_command_arg:argument("mandatory", integer, "mandatort desc")
  ),

  cli_console_command:register(["command"],
                               [SimpleWithDefault, Mandatory],
                               fun(A) -> A end,
                               ""),

  ?assertEqual({ok, [{"mandatory", 12},
                     {"simple", "test_nd"}]},
               cli_console_command:run(["command"], [{"simple", "test_nd"},
                                                          {"mandatory", "12"}])),
  ?assertEqual({ok, [{"mandatory", 1},
                     {"simple", "test"}]},
               cli_console_command:run( ["command"], [{"mandatory", "1"}])).

handle_cast_for_cover(_Config) ->
  gen_server:cast(cli_console_command, test).

arg_convert_happy(_Config) ->
  test_arg_convert(string, "test", "test"),
  test_arg_convert(binary, "test", <<"test">>),
  test_arg_convert(integer, "213", 213),
  test_arg_convert(atom, "fo_bar_1", 'fo_bar_1'),
  test_arg_convert(flag, true, true).


test_arg_convert(Type, Test, Result) ->
  Arg = cli_console_command_arg:argument("test_arg", Type, "desc"),
  cli_console_command:register(["type_check"], [Arg], fun(A) -> A end, ""),
  ?assertEqual({ok, [{"test_arg", Result}]},
               cli_console_command:run(["type_check"], [{"test_arg", Test}])).


