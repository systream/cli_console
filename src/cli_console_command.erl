%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_command).

-behaviour(gen_server).

-include("cli_console_command.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/0, register/4, register/2, run/2, get_help/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE_NAME, cli_commands).
-define(PERSISTENT_TERM_KEY, {?MODULE, register_command_callbacks}).

-record(state, {
  running_commands = [] :: [{CommandPid :: pid(), From :: {pid(), term()}}]
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec register([command()], [command_argument()], command_fun(), string()) ->
  ok | {error, {multiple_argument_definitions, Name :: string()}}.
register(Command, Arguments, Fun, Description) ->
  gen_server:call(?SERVER, {register, Command, Arguments, Fun, Description}).

-spec register(module(), atom()) ->
  ok | {error, {multiple_argument_definitions, Name :: string()}}.
register(Module, Function) ->
  gen_server:call(?SERVER, {register, Module, Function}).

-spec run([string()], proplists:proplist()) ->
  {ok, term()} |
  {error, command_not_found} |
  {error, {not_convertible, {argument_type(), term()}}} |
  {error, {missing_arguments, list(term())}}.
run(Command, Arguments) ->
  gen_server:call(?SERVER, {run, Command, Arguments}, timer:seconds(60)).

-spec get_help([string()]) ->
  {ok, term()}.
get_help(Command) ->
  gen_server:call(?SERVER, {get_help, Command}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link() ->
  {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [],
                        [{hibernate_after, timer:seconds(160)}]).

-spec init(term()) -> {ok, state()}.
init(_) ->
  process_flag(trap_exit, true),
  ets:new(?ETS_TABLE_NAME, [set, protected, named_table]),
  init_commands_from_registered_callback(),
  {ok, #state{}}.

-spec handle_call(term(), {pid(), Tag :: term()}, state()) ->
  {reply, term(), state()} | {noreply, state()}.
handle_call({run, Command, Args}, From, State = #state{}) ->
  case ets:lookup(?ETS_TABLE_NAME, Command) of
    [] ->
      case get_wildcard_command(Command) of
        [] ->
          {reply, {error, command_not_found}, State};
        Result ->
          evaluate_command_lookup_result(Result, Command, Args, From, State)
      end;
    Else ->
      evaluate_command_lookup_result(Else, Command, Args, From, State)
  end;
handle_call({get_help, Command}, _From, State = #state{}) ->
  Help = do_get_help(Command),
  Output = format_help(sort_by_commands(Help)),
  {reply, {ok, [cli_console_formatter:title("Help~n") | Output]}, State};
handle_call({register, Command, ArgsDef, Fun, Description}, _From,
            State = #state{}) ->
  {reply, register_command(Command, ArgsDef, Fun, Description), State};
handle_call({register, Module, Function}, _From, State = #state{}) ->
  case catch register_commands_via_callback([{Module, Function}]) of
    ok ->
      store_command_register_callback(Module, Function),
      {reply, ok, State};
    Else ->
      {reply, {error, Else}, State}
  end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

-spec handle_info(Info :: term(), State :: term()) ->
  {noreply, NewState :: term()}.
handle_info({'EXIT', Pid, normal}, #state{running_commands = Rc} = State) ->
  {noreply, State#state{running_commands = proplists:delete(Pid, Rc)}};
handle_info({'EXIT', Pid, Reason}, #state{running_commands = Rc} = State) ->
  case lists:keyfind(Pid, 1, Rc) of
    false ->
      ok;
    {_, From} ->
      gen_server:reply(From, {error, Reason})
  end,
  {noreply, State#state{running_commands = proplists:delete(Pid, Rc)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
spawn_run_command(Command, Args, ArgsDef, From, Fun) ->
  spawn_link(fun() ->
              CommandResult = run_command(Command, Args, ArgsDef, Fun),
              gen_server:reply(From, CommandResult)
             end).

evaluate_command_lookup_result([{CommandSpec, ArgsDef, Fun, _Description}],
                               Command, Args, From, State) ->
  NewArgs = extract_command_args(CommandSpec, Command, Args),
  CommandPid = spawn_run_command(Command, NewArgs, ArgsDef, From, Fun),
  RunningCommands = State#state.running_commands,
  {noreply, State#state{running_commands = [{CommandPid, From} | RunningCommands]}}.

run_command(Command, Args, ArgsDef, Fun) ->
  case convert(ArgsDef, Args) of
    {ok, NewArgs} ->
      case is_not_in_arg_def_but_set(ArgsDef, Args, "help") of
        true ->
          get_help(Command);
        _ ->
          evaluate_argument_check(ArgsDef, NewArgs, Fun)
      end;
    Else ->
      Else
   end.

execute_fun(Fun, NewArgs) ->
  try
    {ok, Fun(NewArgs)}
  catch _Type:Error:_St ->
    {ok, [
      cli_console_formatter:error("Error occured"),
      cli_console_formatter:error(io_lib:format("~p", [Error]))
    ]}
  end.

evaluate_argument_check(ArgsDef, NewArgs, Fun) ->
  MissingArguments = get_missing_arguments(ArgsDef, NewArgs),
  evaluate_argument_check(MissingArguments, fun() -> execute_fun(Fun, NewArgs) end).

evaluate_argument_check([], Fun) ->
  Fun();
evaluate_argument_check(MissingArguments, _Fun) ->
  {error, {missing_arguments, MissingArguments}}.

add_default_value(ArgsDef, Args) ->
  lists:foldr(fun add_default_arguments/2, Args, ArgsDef).

add_default_arguments(#argument{name = Name, default = Default} = ArgDef, Args) ->
  case has_default_value(ArgDef) andalso is_undefined(ArgDef, Args) of
    true ->
      [{Name, Default} | Args];
    _ ->
      Args
  end.

get_missing_arguments(ArgDefs, Arguments) ->
  [ArgDef || ArgDef <- ArgDefs, is_mandatory(ArgDef),
                                is_undefined(ArgDef, Arguments),
                                not has_default_value(ArgDef)].

is_undefined(#argument{name = Name}, Arguments) ->
  not proplists:is_defined(Name, Arguments).

is_mandatory(#argument{optional = false}) ->
  true;
is_mandatory(#argument{optional = true}) ->
  false.

has_default_value(#argument{default = undefined}) ->
  false;
has_default_value(#argument{default = _}) ->
  true.

convert(ArgsDef, Args) ->
  WithDefault = add_default_value(ArgsDef, Args),
  convert(ArgsDef, WithDefault, []).

convert([], _Args, Acc) ->
  {ok, Acc};
convert([#argument{name = Name, type = Type} | Rest], Args, Acc) ->
  case convert_arg(proplists:lookup_all(Name, Args), Type, Acc) of
    {error, {not_convertible, {Type, Value}}} ->
      {error, {not_convertible, {Name, Type, Value}}};
    NewAcc ->
      convert(Rest, Args, NewAcc)
  end.

convert_arg([], _Type, Acc) ->
  Acc;
convert_arg([{Name, Value} | Rest], Type, Acc) ->
  case convert_arg(Type, Value) of
    {ok, NewValue} ->
      convert_arg(Rest, Type, [{Name, NewValue} | Acc]);
    Else ->
      Else
  end.

convert_arg(atom, Value) when is_list(Value) ->
  {ok, list_to_atom(Value)};
convert_arg(atom, Value) when is_atom(Value) ->
  {ok, Value};
convert_arg(string, Value) when is_list(Value) ->
  {ok, Value};
convert_arg(binary, Value) when is_list(Value) ->
  {ok, unicode:characters_to_binary(Value)};
convert_arg(flag, true) ->
  {ok, true};
convert_arg(flag, false) ->
  {ok, false};
convert_arg(integer, Value) when is_list(Value) ->
  case is_numeric(Value) of
    true ->
      {ok, list_to_integer(Value)};
    _ ->
      {error, {not_convertible, {integer, Value}}}
  end;
convert_arg(integer, Value) when is_number(Value) ->
  {ok, Value};
convert_arg(Type, Value) ->
  {error, {not_convertible, {Type, Value}}}.

-spec is_numeric(string()) -> boolean().
is_numeric([]) ->
  false;
is_numeric(Value) ->
  [Char || Char <- Value, Char >= $0, Char =< $9] =:= Value.

do_get_help([]) ->
  ets:foldr(fun({Command, _, _, Desc}, Acc) ->
              [{Command, Desc} | Acc]
            end, [], ?ETS_TABLE_NAME);
do_get_help(InCommands) ->
  case ets:lookup(?ETS_TABLE_NAME, InCommands) of
    [] ->
      do_help_lookup(InCommands);
    [{Commands, ArgsDef, _Fun, Description}] ->
      [{Commands, Description, ArgsDef}]
  end.

do_help_lookup(InCommands) ->
  case ets:select(?ETS_TABLE_NAME, get_select_matchspec(InCommands, false)) of
    [] ->
      ets:select(?ETS_TABLE_NAME, get_select_matchspec(InCommands, true));
    Else ->
      Else
  end.

get_select_matchspec(InCommands, false) ->
  CommandLength = length(InCommands),
  ets:fun2ms(fun({Command, _, _, Description})
                  when hd(Command) == hd(InCommands) andalso
                       length(Command) > CommandLength ->
                    {Command, Description}
             end);
get_select_matchspec(InCommands, true) ->
  CommandLength = length(InCommands),
  ets:fun2ms(fun({Command, _, _, Description})
                  when length(Command) =< CommandLength ->
                    {Command, Description}
             end).

sort_by_commands(Data) ->
  lists:usort(fun({CommandsA, _}, {CommandsB, _}) ->
                CommandsA =< CommandsB
              end, Data).

format_command({Commands, Desc, []}) ->
  format_command({Commands, Desc});
format_command({Commands, Desc, Args}) ->
  CommandFormat = format_command({Commands, Desc}),
  [CommandFormat,
   cli_console_formatter:text("~nArguments: ") |
   [format_arg(Arg) || Arg <- Args]];
format_command({Commands, Desc}) ->
  NewCommands = lists:map(fun format_command_string/1, Commands),
  CommandStr = string:pad(string:join(NewCommands, " "), 32),
  cli_console_formatter:text("~ts ~ts", [CommandStr, Desc]).

format_command_string(#argument{name = Name}) ->
  "<" ++ Name ++ ">";
format_command_string(Command) ->
  Command.

format_arg(#argument{name = Name, optional = Optional,
                      default = Default, description = Desc}) ->

  CommandStr = string:pad(Name, 30),
  ArgData = get_default_string(Default, get_optional_string(Optional, "")),
  cli_console_formatter:text(" -~ts ~ts~n~ts~ts",
                             [CommandStr, Desc, lists:duplicate(32, " "), ArgData]).

get_default_string(undefined, Acc) ->
  Acc;
get_default_string(Default, Acc) when is_integer(Default) ->
  Acc ++ " Default value: " ++ io_lib:format("~ts", [integer_to_list(Default)]);
get_default_string(Default, Acc) ->
  Acc ++ " Default value: " ++ io_lib:format("~ts", [Default]).

get_optional_string(true, Acc) ->
  Acc;
get_optional_string(false, Acc) ->
  Acc ++ " Mandatory parameter.".

is_not_in_arg_def_but_set(ArgDef, Args, Flag) ->
  proplists:is_defined(Flag, Args) andalso
  not is_argument_defined(Flag, ArgDef).

is_argument_defined(Arg, ArgsDef) ->
  case lists:keyfind(Arg, 2, ArgsDef) of
    false ->
      false;
    _ ->
      true
  end.

register_command(Command, ArgsDef, Fun, Description) ->
  CommandArguments = get_command_arguments(Command),
  case check_multiple_argdefs(ArgsDef ++ CommandArguments) of
    ok ->
      % need to clean up previous commands to make does not register multiple
      % wildcard argument command
      [ets:delete(?ETS_TABLE_NAME, PrevCommand) ||
        {PrevCommand, _, _, _} <- get_wildcard_command(Command)],
      true = ets:insert(?ETS_TABLE_NAME, {Command, ArgsDef, Fun, Description}),
      ok;
    Else ->
      Else
  end.

get_command_arguments(Command) ->
  lists:filter(fun(#argument{}) -> true;
                  (_)           -> false
               end, Command).

get_command_arguments_with_index(Command) ->
  {_, Result} =
    lists:foldl(fun(#argument{} = Item, {ItemSeq, Acc}) ->
                      {ItemSeq+1, [{ItemSeq, Item} | Acc]};
                   (_, {ItemSeq, Acc}) ->
                     {ItemSeq+1, Acc}
                end, {1, []}, Command),
  Result.

check_multiple_argdefs(Argdefs) ->
  check_multiple_argdefs(Argdefs, []).

check_multiple_argdefs([], _) ->
  ok;
check_multiple_argdefs([#argument{name = Name} | Rest], NameAcc) ->
  case lists:member(Name, NameAcc) of
    true ->
      {error, {multiple_argument_definitions, Name}};
    _ ->
      check_multiple_argdefs(Rest, [Name | NameAcc])
  end.

format_help(HelpCommand) ->
  lists:foldr(fun format_help/2, [], HelpCommand).

format_help(HelpCommand, Acc) ->
  case format_command(HelpCommand) of
    Result when is_list(Result) ->
      lists:foldr(fun(Item, Acc2) -> [Item | Acc2] end, Acc, Result);
    Item ->
      [Item | Acc]
  end.

get_wildcard_command(Command) ->
  case ets:select(?ETS_TABLE_NAME, [{generate_match_head(Command),
                                     generate_filter(Command), ['$_']}]) of
    [] ->
      [];
    [{Commands, ArgsDef, Fun, Description}] ->
      CommandArguments = get_command_arguments(Commands),
      [{Commands, ArgsDef ++ CommandArguments, Fun, Description}]
  end.

generate_match_head(Command) ->
  CommandHead = [item_num(ItemNum) || ItemNum <- lists:seq(1, length(Command))],
  {CommandHead, '_', '_', '_'}.

generate_filter(Command) ->
  generate_filter(Command, 1, []).

generate_filter([], _ItemCount, Where) ->
  lists:reverse(Where);
generate_filter([Command | Rest], ItemCount, Where) ->
  Item = item_num(ItemCount),
  Filter = generate_filter(Command, Item),
  generate_filter(Rest, ItemCount+1, [Filter | Where]).

generate_filter(#argument{}, Item) ->
  {'==', argument, {element, 1, Item}};
generate_filter(Command, Item) ->
  {'orelse',
   {'==', Command, Item},
   {'==', argument, {element, 1, Item}}
  }.

item_num(ItemNum) ->
  list_to_atom("$" ++ integer_to_list(ItemNum)).

extract_command_args(CommandSpec, Command, Args) ->
  CommandArgWithIndex = get_command_arguments_with_index(CommandSpec),
  lists:foldl(fun({Index, #argument{name = Name}}, Acc) ->
                   [{Name, lists:nth(Index, Command)} | Acc]
              end, Args, CommandArgWithIndex).

store_command_register_callback(Module, Function) ->
  Callbacks = get_command_register_callbacks(),
  Data = {Module, Function},
  NewCallbacks =
    case lists:member(Data, Callbacks) of
      true ->
        Callbacks;
      false ->
        [Data | Callbacks]
    end,
  persistent_term:put(?PERSISTENT_TERM_KEY, NewCallbacks).

get_command_register_callbacks() ->
  persistent_term:get(?PERSISTENT_TERM_KEY, []).

init_commands_from_registered_callback() ->
  register_commands_via_callback(get_command_register_callbacks()).

register_commands_via_callback([]) ->
  ok;
register_commands_via_callback([{Module, Function} | Rest]) ->
  Result = apply(Module, Function, []),
  register_command_callback_result(Result),
  register_commands_via_callback(Rest).

register_command_callback_result([]) ->
  ok;
register_command_callback_result([{Command, ArgDef, Fun, Description} | Rest]) ->
  ok = register_command(Command, ArgDef, Fun, Description),
  register_command_callback_result(Rest).
