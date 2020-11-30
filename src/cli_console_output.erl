%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_output).
-author("Peter Tihanyi").

-include("cli_console_command.hrl").

-callback format(term()) -> string().

%% API
-export([output/1]).

-spec output({ok, [output_format()] | output_format()} | {error, Error}) -> ok when
  Error :: command_not_found |
           {not_convertible, {Type :: atom(), Value :: term()}} |
           {missing_arguments, [command_argument()]}.
output({error, Error}) ->
  show_error(Error),
  io:format("~n", []),
  ok;
output({ok, Data}) when is_list(Data) ->
  [format(Unit) || Unit <- Data],
  ok;
output({ok, Data}) ->
  output({ok, [Data]}).

-spec format([output_format()] | output_format()) -> ok.
format({text, Data}) ->
  io:format(Data);
format({format, Module, Format}) ->
  io:format(Module:format(Format)).

-spec show_error(command_not_found |
                {not_convertible, {Type :: atom(), Value :: term()}} |
                {missing_arguments, [command_argument()]}) -> ok.
show_error(command_not_found) ->
  io:format("Command not found~n");
show_error({not_convertible, {Name, Type, Value}}) ->
  io:format("Illegal parameter: ~s (~p) - ~p~n", [Name, Type, Value]);
show_error({missing_arguments, Args}) ->
  io:format("Missing argument: ~n", []),
  [missing_argument(Arg) || Arg <- Args],
  ok;
show_error(Error) ->
  io:format("~p ~n", [Error]),
  ok.

-spec missing_argument(command_argument()) -> ok.
missing_argument(#argument{name = Name, description = Desc}) ->
  NameStr = string:pad(Name, 19),
  io:format(" * ~s \t ~s~n", [NameStr, Desc]).