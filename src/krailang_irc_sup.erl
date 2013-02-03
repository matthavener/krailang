-module(krailang_irc_sup).
-behaviour(supervisor).

-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

start_link(Args) ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
        AChild = {krailang_irc, {krailang_irc, start_link, [Args]},
                          permanent, 5000, worker, [krailang_irc]},

        {ok, { {one_for_one, 5, 10}, [AChild]}}.


