%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cli_console_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(Args :: term()) ->
  {ok, {SupFlags :: supervisor:sup_flags(),
        [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 100},
    ChildSpecs = [
                  #{id => cli_command_server,
                    start => {cli_console_command, start_link, []},
                    restart => permanent,
                    modules => [cli_console_command]
                  }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
