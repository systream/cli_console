%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console).
-author("Peter Tihanyi").

-include("cli_console_command.hrl").

%% API
-export([run/1, register/4]).

%% @doc Execute commands
-spec run(string()) -> ok.
run(ConsoleCommand) ->
  {ok, Command, Arguments} = cli_console_parser:parse(ConsoleCommand),
  Result = cli_console_command:run(Command, Arguments),
  cli_console_output:output(Result),
  maybe_print_help(Result, Command).

%% @doc Register command
-spec register([command()], [command_argument()], command_fun(), string()) -> ok.
register(Command, Arguments, Fun, Description) ->
  cli_console_command:register(Command, Arguments, Fun, Description).

maybe_print_help({error, _}, Command) ->
  cli_console_output:output(cli_console_command:get_help(Command));
maybe_print_help(_, _) ->
  ok.
