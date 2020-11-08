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
    parameter
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

  All = cli_console_command_arg:argument("all", flag),
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

  ?assertEqual("Command not found\n",
               catch_output(fun() -> cli_console:run("unknown command") end)),
  ?assertEqual("Commands\n * show tables: list tales\n * help: print help\n",
               catch_output(fun() -> cli_console:run("show commands") end)),
  ?assertEqual("\e[37;1mTables\n\n\e[0m * users\n * permissions\n * tweats\n",
               catch_output(fun() -> cli_console:run("show tables") end)),
  ?assertEqual("\e[37;1mTables\n\n\e[0mListing all tables\n---------------------------------------------------------------\n * users\n * permissions\n * tweats\n",
               catch_output(fun() -> cli_console:run("show tables --all") end)).


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
  ok = cli_console:register(["help"], [],
                            fun(_Args) ->
                              {text, "This is the help"}
                            end, "print help"),


  ?assertEqual("This is the help",
               catch_output(fun() -> cli_console:run(" help") end)).

missing_parameter(_Config) ->
  Table = cli_console_command_arg:argument("table", string),
  Row = cli_console_command_arg:argument("Row", string, "Desc for row"),
  ok = cli_console:register(["delete"],
                            [cli_console_command_arg:mandatory(Table),
                             cli_console_command_arg:mandatory(Row)],
                            fun(_Args) ->
                              [{text, "success"}]
                            end,
                            "Delete Table"),


  ?assertEqual("Missing argument: \n * \"table\"\n * \"Row\": \"Desc for row\"\n",
               catch_output(fun() -> cli_console:run("delete") end)).

bad_parameter(_Config) ->
  Threshold = cli_console_command_arg:argument("threshold", integer),
  ok = cli_console:register(["set", "limit"], [Threshold],
                            fun(_Args) ->
                              [{text, "success"}]
                            end,
                            "set limit for something"),


  ?assertEqual("Illegal parameter: integer \"abc\"\n",
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
  catch Exception ->
    throw(Exception)
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