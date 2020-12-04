%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cli_console_app).

-behaviour(application).

-include("cli_console_command.hrl").

-export([start/2, stop/1, get_help/1]).

-spec start(StartType :: term(), StartArgs :: term()) ->
    {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.
start(_StartType, _StartArgs) ->
    Ret = {ok, _} = cli_console_sup:start_link(),
    cli_console:register(["help"], [],
                         fun cli_console_app:get_help/1, "Print help"),
    Ret.

-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.

-spec get_help(term()) -> output_format().
get_help(_) ->
    {ok, Data} = cli_console_command:get_help([]),
    Data.
