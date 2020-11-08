%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_command_arg).
-author("Peter Tihanyi").

-include("cli_console_command.hrl").

%% API
-export([argument/2, argument/3, set_default/2, mandatory/1]).

%% @doc create argument record
-spec argument(string(), argument_type()) -> command_argument().
argument(Name, Type) ->
  #argument{name = Name, type = Type}.

%% @doc create argument record with description
-spec argument(string(), argument_type(), string()) -> command_argument().
argument(Name, Type, Description) ->
  #argument{name = Name, type = Type, description = Description}.

%% @doc set default value for argument
-spec set_default(command_argument(), term()) -> command_argument().
set_default(Argument = #argument{}, Default) ->
  Argument#argument{default = Default}.

%% @doc Set argument as mandatory
-spec mandatory(command_argument()) -> command_argument().
mandatory(Argument = #argument{}) ->
  Argument#argument{optional = false}.
