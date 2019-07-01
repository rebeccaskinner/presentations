-module(sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    AChild = #{id => 'AName',
               start => {'AModule', start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => ['AModule']},
    {ok, {SupFlags, [AChild]}}.
