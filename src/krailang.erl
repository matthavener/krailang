-module(krailang).
-behaviour(gen_server).

%-export([start_link/0]).
-compile(export_all).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {bot}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
%% gen_server callbacks

init([]) ->
    {ok, Bot} = krailang_bot:start_link(),
    {ok, _} = krailang_irc_sup:start_link([self(),"irc.collegiumv.org", 8080, "krailOS", "#main"]),
    {ok, #state{bot=Bot}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, #state{bot=Bot} = State) ->
    case Info of 
        {irc, Irc, User, Msg} ->
            Rs = krailang_bot:make_responses(Bot, Msg),
            lists:map(fun(R) -> Irc ! {irc, say, User ++ ": " ++ R} end, Rs); 
        Other ->
            io:format("weird ~p ~p ~n", [Other, State])
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



