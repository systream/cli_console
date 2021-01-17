%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, Systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_SUITE).
-author("Pete Tihanyi").

%% API
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


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
  meck:unload(),
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
  [
    simple_command,
    table,
    simple_text,
    missing_parameter,
    bad_parameter,
    parameter,
    automatic_help_argument,
    automatic_help_command,
    help_for_exact_command,
    help_subcommands,
    empty_command_for_full_help,
    handle_errors,
    crash_in_function,
    default_value_flag,
    pid_not_in_state,
    wild_card_argument_help,
    register_callback_ok,
    register_callback_empty,
    register_callback_bad_return,
    register_callback_restart
  ].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
simple_command(_Config) ->

  All = cli_console_command_arg:argument("all", flag, "all desc"),
  ok = cli_console:register(["show", "tables"], [All],
    fun(Args) ->
      Init =
        case proplists:is_defined("all", Args) of
          true ->
            [cli_console_formatter:title("Tables~n"),
              cli_console_formatter:text("Listing all tables"),
              cli_console_formatter:separator()];
          _ ->
            [cli_console_formatter:title("Tables~n")]
        end,
      Init ++ [cli_console_formatter:list(["users", "permissions", "tweats"])]
    end,
    "show database tables"),

  ok = cli_console:register(["show", "commands"], [],
    fun(_) ->
      [
        cli_console_formatter:text("Commands"),
        cli_console_formatter:list(["show tables: list tales",
                                    "help: print help"])
       ]
    end,
    "show available commands"),

  ?assertEqual("Command not found\n\n\e[37;1mHelp\n\n\e[0mhelp                             Print help\nshow commands                    show available commands\nshow tables                      show database tables\n",
               catch_output(fun() -> cli_console:run("unknown command") end)),
  ?assertEqual("Command not found\n\n\e[37;1mHelp\n\n\e[0mhelp                             Print help\nshow commands                    show available commands\nshow tables                      show database tables\n",
               catch_output(fun() -> cli_console:run(["unknown", "command"]) end)),
  ?assertEqual("Commands\n * show tables: list tales\n * help: print help\n",
               catch_output(fun() -> cli_console:run("show commands") end)),
  ?assertEqual("Commands\n * show tables: list tales\n * help: print help\n",
               catch_output(fun() -> cli_console:run(["show", "commands"]) end)),
  ?assertEqual("\e[37;1mTables\n\n\e[0m * users\n * permissions\n * tweats\n",
               catch_output(fun() -> cli_console:run("show tables") end)),
  ?assertEqual("\e[37;1mTables\n\n\e[0m * users\n * permissions\n * tweats\n",
               catch_output(fun() -> cli_console:run(["show", "tables"]) end)),
  ?assertEqual("\e[37;1mTables\n\n\e[0mListing all tables\n---------------------------------------------------------------\n * users\n * permissions\n * tweats\n",
               catch_output(fun() -> cli_console:run("show tables --all") end)),
  ?assertEqual("\e[37;1mTables\n\n\e[0mListing all tables\n---------------------------------------------------------------\n * users\n * permissions\n * tweats\n",
               catch_output(fun() -> cli_console:run(["show", "tables", "--all"]) end)).


table(_Config) ->
  ok = cli_console:register(["describe", "table"], [],
          fun(_Args) ->

            cli_console_formatter:table([
                #{"field_a" => "A", "field_b" => "B"},
                #{"field_a" => "A", "field_b" => "C"},
                #{"field_c" => "ok", "field_a" => "no ok"},
                #{"field_g" => "totally disabled"}
                                        ])
          end,
          "list all fields in table"),

  %% should look something like this
  %%  --------------------------------------------------------------
  %% |  field_a   |  field_b   |  field_c   |       field_g       |
  %% --------------------------------------------------------------
  %% |     A      |     B      |            |                     |
  %% |     A      |     C      |            |                     |
  %% |   no ok    |            |     ok     |                     |
  %% |            |            |            |  totally disabled   |
  %% --------------------------------------------------------------
  %% try with io:format("~s", [Output]),

  Output = catch_output(fun() -> cli_console:run("describe table") end),
  ?assertEqual(" -------------------------------------------------------------- \n | \e[37m field_a  \e[0m | \e[37m field_b  \e[0m | \e[37m field_c  \e[0m | \e[37m      field_g      \e[0m |\n -------------------------------------------------------------- \n |     A      |     B      |            |                     |\n |     A      |     C      |            |                     |\n |   no ok    |            |     ok     |                     |\n |            |            |            |  totally disabled   |\n -------------------------------------------------------------- \n",
               Output).

simple_text(_Config) ->
  ok = cli_console:register(["test", "help"], [],
                            fun(_Args) ->
                              {text, "This is the help"}
                            end, "print help"),


  ?assertEqual("This is the help",
               catch_output(fun() -> cli_console:run("  test  help") end)).

missing_parameter(_Config) ->
  Table = cli_console_command_arg:argument("table", string, "table desc"),
  Row = cli_console_command_arg:argument("Row", string, "Desc for row"),
  ok = cli_console:register(["delete"],
                            [cli_console_command_arg:mandatory(Table),
                             cli_console_command_arg:mandatory(Row)],
                            fun(_Args) ->
                              [{text, "success"}]
                            end,
                            "Delete Table"),


  ?assertEqual("Missing argument: \n * table               \t table desc\n * Row                 \t Desc for row\n\n",
               catch_output(fun() -> cli_console:run("delete") end)).

bad_parameter(_Config) ->
  Threshold = cli_console_command_arg:argument("threshold", integer,
                                               "threshold desc"),
  ok = cli_console:register(["set", "limit"], [Threshold],
                            fun(_Args) ->
                              [{text, "success"}]
                            end,
                            "set limit for something"),


  ?assertEqual("Illegal parameter: threshold (integer) - \"abc\"\n\n",
               catch_output(fun() ->
                              cli_console:run("set limit --threshold=abc")
                            end)).

parameter(_Config) ->
  All = cli_console_command_arg:argument("all", flag, "List all partitions"),
  Node = cli_console_command_arg:set_default(
    cli_console_command_arg:argument("node", atom, "Target node name"), node()
  ),
  Limit = cli_console_command_arg:mandatory(
    cli_console_command_arg:argument("limit", integer, "Max number of items to show")
  ),
  cli_console:register(["list", "partitions"],
                       [All, Node, Limit],
                       fun list_partitions/1,
                       "List all partitions"),
  Output =
    catch_output(fun() ->
                  cli_console:run("list partitions --limit 123 -node=test")
                end),
  ?assertEqual("\e[37;1mList of partions\n\e[0m---------------------------------------------------------------\nNode: test\nLimit: 123\n ------------------------------- \n | \e[37m    node    \e[0m | \e[37m partition  \e[0m |\n ------------------------------- \n |  node@test   |      a       |\n |  node@test   |      b       |\n |  node@test   |      c       |\n |  node@test   |      d       |\n ------------------------------- \n",
               Output).

automatic_help_argument(_Config) ->
  Help = cli_console_command_arg:argument("help", flag, "print help"),
  TestFun = fun(_) -> [{text, "ok"}] end,
  cli_console:register(["test"], [], TestFun, "List all partitions"),
  Output = catch_output(fun() -> cli_console:run("test --help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mtest                             List all partitions\n",
               Output),

  TestArg = cli_console_command_arg:argument("foo", flag, "foo flag"),
  cli_console:register(["test"], [TestArg], TestFun, "List all partitions"),
  OutputFlag = catch_output(fun() -> cli_console:run("test --help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mtest                             List all partitions\n\nArguments: \n -foo                            foo flag\n                                \n",
               OutputFlag),

  cli_console:register(["test"],
                       [cli_console_command_arg:set_default(TestArg, true)],
                       TestFun, "List all partitions"),
  OutputFlagDefault = catch_output(fun() -> cli_console:run("test --help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mtest                             List all partitions\n\nArguments: \n -foo                            foo flag\n                                 Default value: true\n",
               OutputFlagDefault),

  cli_console:register(["test"],
                       [cli_console_command_arg:mandatory(TestArg)],
                       TestFun, "List all partitions"),
  OutputFlagMandatory = catch_output(fun() -> cli_console:run("test --help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mtest                             List all partitions\n\nArguments: \n -foo                            foo flag\n                                 Mandatory parameter.\n",
               OutputFlagMandatory),

  cli_console:register(["test"], [Help], TestFun, "List all partitions"),
  Output2 = catch_output(fun() -> cli_console:run("test --help") end),
  ?assertEqual("ok", Output2).

automatic_help_command(_Config) ->
  TestFun = fun(_) -> [{text, "ok"}] end,
  cli_console:register(["test"], [], TestFun, "Test fun desc"),
  Output = catch_output(fun() -> cli_console:run("help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mdelete                           Delete Table\ndescribe table                   list all fields in table\nhelp                             Print help\nlist partitions                  List all partitions\nset limit                        set limit for something\nshow commands                    show available commands\nshow tables                      show database tables\ntest                             Test fun desc\ntest help                        print help\n",
               Output).

help_for_exact_command(_Config) ->
  TestFun = fun(_) -> [{text, "ok"}] end,
  % no args
  cli_console:register(["test"], [], TestFun, "Test fun desc"),
  Output = catch_output(fun() -> cli_console:run("test -help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mtest                             Test fun desc\n",
               Output),
  Args =
    [ cli_console_command_arg:set_default(cli_console_command_arg:argument("all", flag, "list all"), false),
      cli_console_command_arg:set_default(cli_console_command_arg:argument("limit", integer, "set number of items"), 10),
      cli_console_command_arg:argument("skip_echo", flag, "skip echo desc"),
      cli_console_command_arg:mandatory(cli_console_command_arg:argument("toggle", flag, "Toggle for testpurposes")),
      cli_console_command_arg:set_default(cli_console_command_arg:mandatory(cli_console_command_arg:argument("allin", flag, "Mandatory and has default value it doesnotmake sence but itis possible for test purposes")), true)
    ],

  cli_console:register(["test"], Args, TestFun, "Test fun desc"),
  Output2 = catch_output(fun() -> cli_console:run("test -help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mtest                             Test fun desc\n\nArguments: \n -all                            list all\n                                 Default value: false\n -limit                          set number of items\n                                 Default value: 10\n -skip_echo                      skip echo desc\n                                \n -toggle                         Toggle for testpurposes\n                                 Mandatory parameter.\n -allin                          Mandatory and has default value it doesnotmake sence but itis possible for test purposes\n                                 Mandatory parameter. Default value: true\n",
               Output2).

help_subcommands(_Config) ->
  TestFun = fun(_) -> [{text, "ok"}] end,
  cli_console:register(["foo", "bar"], [], TestFun, "foo bar fun desc"),
  cli_console:register(["foo", "test"], [], TestFun, "foo test fun desc"),
  Output = catch_output(fun() -> cli_console:run("foo") end),
  ?assertEqual("Command not found\n\n\e[37;1mHelp\n\n\e[0mfoo bar                          foo bar fun desc\nfoo test                         foo test fun desc\n",
               Output).

empty_command_for_full_help(_Config) ->
  Output = catch_output(fun() -> cli_console:run("") end),
  ?assertEqual("Command not found\n\n\e[37;1mHelp\n\n\e[0mdelete                           Delete Table\ndescribe table                   list all fields in table\nfoo bar                          foo bar fun desc\nfoo test                         foo test fun desc\nhelp                             Print help\nlist partitions                  List all partitions\nset limit                        set limit for something\nshow commands                    show available commands\nshow tables                      show database tables\ntest                             Test fun desc\ntest help                        print help\n",
               Output).

handle_errors(_Config) ->
  TestFun = fun(_) -> throw("ooups"), [{text, "ok"}] end,
  cli_console:register(["test"], [], TestFun, "List all partitions"),
  Output = catch_output(fun() -> cli_console:run("test") end),
  ?assertEqual("\e[31;1mError occured\n\e[0m\e[31;1m\"ooups\"\n\e[0m", Output).

crash_in_function(_Config) ->
  ok = meck:new(unicode, [passthrough, unstick]),
  meck:expect(unicode, characters_to_binary,
              fun(_) -> throw(big_error_in_conveer) end),
  TestFun = fun(_) -> [{text, "ok"}] end,
  Arg1 = cli_console_command_arg:argument("arg1", binary, "Arg1 desc"),
  cli_console:register(["test"], [Arg1], TestFun, "List all partitions"),
  Output = catch_output(fun() -> cli_console:run("test --arg1=foobar") end),

  ?assertEqual("{nocatch,big_error_in_conveer}",
               string:sub_string(Output, 2, 31)).

pid_not_in_state(_Config) ->
  erlang:whereis(cli_console_command) ! {'EXIT', self(), error},
  ok.

default_value_flag(_Config) ->
  cli_console:register(["connections"],
                       [
                         cli_console_command_arg:set_default(
                           cli_console_command_arg:argument("all", flag, "desc"),
                           false),
                         cli_console_command_arg:set_default(
                           cli_console_command_arg:argument("limit", integer, "desc"),
                           10
                         ),
                         cli_console_command_arg:argument("skip_echo", flag, "desc")
                       ],
                       fun (_) -> {text, "ok"} end,
                       "List connections status"),
  Output = catch_output(fun() -> cli_console:run("connections --help") end),
  ?assertEqual("\e[37;1mHelp\n\n\e[0mconnections                      List connections status\n\nArguments: \n -all                            desc\n                                 Default value: false\n -limit                          desc\n                                 Default value: 10\n -skip_echo                      desc\n                                \n",
               Output).

wild_card_argument_help(_Config) ->
  Fun = fun(_Args) -> [{text, "ok"}] end,
  Arg1 = cli_console_command_arg:argument("table", string, "Table name"),
  Arg2 = cli_console_command_arg:argument("type", string, "Table type"),
  ok = cli_console_command:register(["wild", Arg1, Arg2], [], Fun, "Wild help"),
  ?assertEqual("Command not found\n\n\e[37;1mHelp\n\n\e[0mwild <table> <type>              Wild help\n",
               catch_output(fun() -> cli_console:run(["wild"]) end)).

register_callback_ok(_Config) ->
  Fun = fun(_Args) -> [{text, "ok"}] end,
  Arg1 = cli_console_command_arg:argument("arg1", string, "Arg1 desc"),

  ok = meck:new(cli_test_callback, [non_strict, passthrough]),
  meck:expect(cli_test_callback, register_cli,
              fun() -> [{["test", "cb"], [Arg1], Fun, "Test cb help"}] end),

  ?assertEqual(ok, cli_console:register(cli_test_callback, register_cli)),
  ?assertEqual("ok",
               catch_output(fun() -> cli_console:run("test cb --arg1=ok") end)).

register_callback_empty(_Config) ->
  ok = meck:new(cli_test_callback, [non_strict, passthrough]),
  meck:expect(cli_test_callback, register_cli, fun() -> [] end),

  ?assertEqual(ok, cli_console:register(cli_test_callback, register_cli)).

register_callback_bad_return(_Config) ->
  ok = meck:new(cli_test_callback, [non_strict, passthrough]),
  meck:expect(cli_test_callback, register_cli, fun() -> foo end),

  ?assertMatch({error, _}, cli_console:register(cli_test_callback, register_cli)).

register_callback_restart(Config) ->
  register_callback_ok(Config),

  % restart command sever
  [exit(Pid, kill) ||
    {Id, Pid, worker, _} <- supervisor:which_children(cli_console_sup),
    Id =:= cli_command_server],

  % wait a litle to supervisor restart the died process
  ct:sleep(200),
  ?assertEqual("ok",
               catch_output(fun() -> cli_console:run("test cb --arg1=ok") end)).


list_partitions(Args) ->
  [cli_console_formatter:title("List of partions"),
   cli_console_formatter:separator(),
   cli_console_formatter:text("Node: ~p", [proplists:get_value("node", Args)]),
   cli_console_formatter:text("Limit: ~p", [proplists:get_value("limit", Args)]),
   cli_console_formatter:table(get_partitions(proplists:get_value("all", Args, false)))
  ].

get_partitions(true) ->
  [#{"partition" => [P], "node" => "node@test"} || P <- lists:seq($a, $z)];
get_partitions(false) ->
  [#{"partition" => [P], "node" => "node@test"} || P <- lists:seq($a, $d)].


catch_output(Fun) ->
  GroupLeader = erlang:group_leader(),
  NewGroupLeader = spawn(fun temporary_group_leader/0),
  try
    erlang:group_leader(NewGroupLeader, self()),
    Fun()
  after
    erlang:group_leader(GroupLeader, self())
  end,
  NewGroupLeader ! {get_data, self()},
  receive
    {data, Data} ->
      FormattedResponse = [io_lib:format(Text, Params) || [Text, Params] <- Data],
      lists:flatten(lists:reverse(FormattedResponse))
  after 1000 ->
    timeout
  end.

temporary_group_leader() ->
  temporary_group_leader([]).

temporary_group_leader(Acc) ->
  receive
    {io_request, From, ReplyAs, {put_chars, unicode, io_lib, format, Data}} ->
      From ! {io_reply, ReplyAs, ok},
      temporary_group_leader([Data | Acc]);
    {get_data, Pid} ->
      Pid ! {data, Acc}
  end.