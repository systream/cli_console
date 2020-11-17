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

-export([start_link/0, register/4, run/2, get_help/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE_NAME, cli_commands).

-record(state, {}).

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
  ets:new(cli_commands, [set, protected, named_table]),
  {ok, #state{}}.

-spec handle_call(term(), {pid(), Tag :: term()}, state()) ->
  {reply, term(), state()} | {noreply, state()}.
handle_call({run, Command, Args}, From, State = #state{}) ->
  case ets:lookup(?ETS_TABLE_NAME, Command) of
    [] ->
      {reply, {error, command_not_found}, State};
    [{_, ArgsDef, Fun, _Description}] ->
      run_command(Command, Args, ArgsDef, From, Fun),
      {noreply, State}
  end;
handle_call({get_help, Command}, _From, State = #state{}) ->
  Help = do_get_help(Command),
  Output = [format_command(HelpCommand) || HelpCommand <- sort_by_commands(Help)],
  {reply, {ok, [cli_console_formatter:title("Help~n") | Output]}, State};
handle_call({register, Command, ArgsDef, Fun, Description}, _From,
            State = #state{}) ->
  true = ets:insert(?ETS_TABLE_NAME, {Command, ArgsDef, Fun, Description}),
  {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

run_command(Command, Args, ArgsDef, From, Fun) ->
  spawn(fun() ->
          MissingArguments = get_missing_arguments(ArgsDef, Args),
          Result =
            evalute_argument_check(MissingArguments,
                                  fun() ->
                                    WithDefault = add_default_value(ArgsDef, Args),
                                    case convert(ArgsDef, WithDefault) of
                                      {ok, NewArgs} ->
                                        case is_not_in_arg_def_but_set(ArgsDef,
                                                                       Args,
                                                                       "help") of
                                          true ->
                                            get_help(Command);
                                          _ ->
                                            {ok, Fun(NewArgs)}
                                        end;
                                      Else ->
                                        Else
                                    end
                                  end),
          gen_server:reply(From, Result)
        end).

evalute_argument_check([], Fun) ->
  Fun();
evalute_argument_check(MissingArguments, _Fun) ->
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
  convert(ArgsDef, Args, []).

convert([], _Args, Acc) ->
  {ok, Acc};
convert([#argument{name = Name, type = Type} | Rest], Args, Acc) ->
  case proplists:get_value(Name, Args) of
    undefined ->
      convert(Rest, Args, Acc);
    Value ->
      case convert_arg(Type, Value) of
        {ok, NewValue} ->
          convert(Rest, Args, [{Name, NewValue} | Acc]);
        Else ->
          Else
      end
  end.

convert_arg(atom, Value) when is_list(Value) ->
  {ok, list_to_atom(Value)};
convert_arg(string, Value) when is_list(Value) ->
  {ok, Value};
convert_arg(binary, Value) when is_list(Value) ->
  {ok, list_to_binary(Value)};
convert_arg(flag, true) ->
  {ok, true};
convert_arg(integer, Value) when is_list(Value) ->
  case is_numeric(Value) of
    true ->
      {ok, list_to_integer(Value)};
    _ ->
      {error, {not_convertible, {integer, Value}}}
  end;
convert_arg(Type, Value) ->
  {error, {not_convertible, {Type, Value}}}.

-spec is_numeric(string()) -> boolean().
is_numeric(Value) ->
  [Char || Char <- Value, Char >= $0, Char =< $9] =:= Value.

do_get_help(Commands) ->
  Match = {'$1', '_', '_', '$2'},
  Result = ['$1', '$2'],
  CommandLength = length(Commands),
  Filter = [{'=<',{length, '$1'}, CommandLength}],
  ets:select(cli_commands, [{Match, Filter, [Result]}]).

sort_by_commands(Data) ->
  lists:usort(fun([CommandsA, _], [CommandsB, _]) ->
                CommandsA =< CommandsB
              end, Data).

format_command([Commands, Desc]) ->
  CommandStr = string:pad(string:join(Commands, " "), 27),
  cli_console_formatter:text("~s \t ~s", [CommandStr, Desc]).

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