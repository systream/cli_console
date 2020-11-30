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

-export([start_link/0, register/4, run/2, get_help/1, handle_info/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE_NAME, cli_commands).

-record(state, {
  running_commands = [] :: [{CommandPid :: pid(), From :: {pid(), term()}}]
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec register([command()], [command_argument()], command_fun(), string()) -> ok.
register(Command, Arguments, Fun, Description) ->
  gen_server:call(?SERVER, {register, Command, Arguments, Fun, Description}).

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

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init(term()) -> {ok, state()}.
init(_) ->
  process_flag(trap_exit, true),
  ets:new(cli_commands, [set, protected, named_table]),
  {ok, #state{}}.

-spec handle_call(term(), {pid(), Tag :: term()}, state()) ->
  {reply, term(), state()} | {noreply, state()}.
handle_call({run, Command, Args}, From, State = #state{}) ->
  case ets:lookup(?ETS_TABLE_NAME, Command) of
    [] ->
      {reply, {error, command_not_found}, State};
    [{_, ArgsDef, Fun, _Description}] ->
      CommandPid = run_command(Command, Args, ArgsDef, From, Fun),
      RunningCommands = State#state.running_commands,
      {noreply, State#state{running_commands = [{CommandPid, From} | RunningCommands]}}
  end;
handle_call({get_help, Command}, _From, State = #state{}) ->
  Help = do_get_help(Command),
  Output = format_help(sort_by_commands(Help)),
  {reply, {ok, [cli_console_formatter:title("Help~n") | Output]}, State};
handle_call({register, Command, ArgsDef, Fun, Description}, _From,
            State = #state{}) ->
  {reply, register_command(Command, ArgsDef, Fun, Description), State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

-spec handle_info(Info :: term(), State :: term()) ->
  {noreply, NewState :: term()}.
handle_info({'EXIT', Pid, normal}, #state{running_commands = Rc} = State) ->
  {noreply, State#state{running_commands = lists:delete(Pid, Rc)}};
handle_info({'EXIT', Pid, Reason}, #state{running_commands = Rc} = State) ->
  case lists:keyfind(Pid, 1, Rc) of
    false ->
      ok;
    {_, From} ->
      gen_server:reply(From, {error, Reason})
  end,
  {noreply, State#state{running_commands = lists:delete(Pid, Rc)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_command(Command, Args, ArgsDef, From, Fun) ->
  spawn_link(fun() ->
    Result =
      case convert(ArgsDef, Args) of
        {ok, NewArgs} ->
          case is_not_in_arg_def_but_set(ArgsDef, Args, "help") of
            true ->
              get_help(Command);
            _ ->
              MissingArguments = get_missing_arguments(ArgsDef, NewArgs),
              evaluate_argument_check(MissingArguments, fun() -> execute_fun(Fun, NewArgs) end)
          end;
        Else ->
          Else
       end,
      gen_server:reply(From, Result)
    end).

execute_fun(Fun, NewArgs) ->
  try
    {ok, Fun(NewArgs)}
  catch _Type:Error:_St ->
    {ok, [
      cli_console_formatter:error("Error occured"),
      cli_console_formatter:error(io_lib:format("~p", [Error]))
    ]}
  end.

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
            end, [], cli_commands);
do_get_help(InCommands) ->
  case ets:lookup(?ETS_TABLE_NAME, InCommands) of
    [] ->
      do_help_lookup(InCommands);
    [{Commands, ArgsDef, _Fun, Description}] ->
      [{Commands, Description, ArgsDef}]
  end.

do_help_lookup(InCommands) ->
  case ets:select(cli_commands, get_select_matchspec(InCommands, false)) of
    [] ->
      ets:select(cli_commands, get_select_matchspec(InCommands, true));
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
  CommandStr = string:pad(string:join(Commands, " "), 32),
  cli_console_formatter:text("~s ~s", [CommandStr, Desc]).

format_arg(#argument{name = Name, optional = Optional,
                      default = Default, description = Desc}) ->

  CommandStr = string:pad(Name, 30),
  ArgData = get_default_string(Default, get_optional_string(Optional, "")),
  cli_console_formatter:text(" -~s ~s~n~s~s",
                             [CommandStr, Desc, lists:duplicate(32, " "), ArgData]).

get_default_string(undefined, Acc) ->
  Acc;
get_default_string(Default, Acc) when is_integer(Default) ->
  Acc ++ " Default value: " ++ io_lib:format("~s", [integer_to_list(Default)]);
get_default_string(Default, Acc) ->
  Acc ++ " Default value: " ++ io_lib:format("~s", [Default]).

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
  case check_multiple_argdefs(ArgsDef) of
    ok ->
      true = ets:insert(?ETS_TABLE_NAME, {Command, ArgsDef, Fun, Description}),
      ok;
    Else ->
      Else
  end.

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