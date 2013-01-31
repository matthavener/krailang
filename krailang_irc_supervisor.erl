-module(krailang_irc_supervisor).
-behaviour(supervisor).

-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

start_link(Args) ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, [self() | Args]).

init(Args) ->
        RestartStrategy = one_for_one,
        MaxRestarts = 100,
        MaxSecondsBetweenRestarts = 300,

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Restart = permanent,
        Shutdown = 2000,
        Type = worker,

        AChild = {krailang_irc, {krailang_irc, start_link, [Args]},
                          Restart, Shutdown, Type, [krailang_irc]},

        {ok, {SupFlags, [AChild]}}.


