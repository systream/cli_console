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
  Result = cli_console_command:run(cli_console_parser:parse(ConsoleCommand)),
  cli_console_output:output(Result).

%% @doc Register command
-spec register([command()], [command_argument()], command_fun(), string()) -> ok.
register(Command, Arguments, Fun, Description) ->
  cli_console_command:register(Command, Arguments, Fun, Description).

