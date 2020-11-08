%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cli_console_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(StartType :: term(), StartArgs :: term()) ->
    {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.
start(_StartType, _StartArgs) ->
    cli_console_sup:start_link().

-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.
