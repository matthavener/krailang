-module(krailang_bot).
-behaviour(gen_server).
%-export([start_link/1]).
-compile(export_all).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {opinions, timers}).
-define(CSV_URL, "http://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AoVJwKRm4drQdDdBY2hQWVBiSGtrMWsycGZzM0hKM3c&single=true&gid=0&output=csv"). 

%% interface

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
make_responses(Pid, Txt) ->
    gen_server:call(Pid, {make_responses, Txt}).
get_opinions(Pid) ->
    gen_server:call(Pid, {get_opinions}).

%% gen_server callbacks

init([] = Args) ->
    io:format("starting bot~p~n", [Args]),
%    timer:sleep(1000),
%    {ok, Opinions} = parse_opinions(get_opinions_sync()),
    Opinions=[],
    {ok, #state{opinions=Opinions, timers=[]}}.

handle_call({make_responses, Txt}, _From, #state{opinions=Opinions,timers=Timers} = State) ->
    {Responses, OpinionsFired} = make_responses_(Opinions, Txt),
    NewTimers = [ erlang:start_timer(1000 * 60 * 60 * 3, self(), {readd_opinion, O}) || O <- OpinionsFired ],
    NewOpinions = lists:subtract(Opinions, OpinionsFired),
    {reply,  Responses, State#state{opinions=NewOpinions,timers=NewTimers ++ Timers}};
handle_call({get_opinions}, _From, #state{timers=Timers}) ->
    [erlang:cancel_timer(T) || T <- Timers],
    {ok, Opinions} = parse_opinions(get_opinions_sync()),
    {reply, {ok, length(Opinions)}, #state{opinions=Opinions, timers=[]}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, #state{opinions=Opinions, timers=Timers} = State) ->
    case Info of 
        {timeout, T, {readd_opinion, O}} ->
            true = lists:any(fun(Tl) -> Tl == T end, Timers), % crash on race 
            {noreply, State#state{opinions=[O|Opinions]}};
        Other ->
            io:format("weird ~p ~p ~n", [Other, State]),
            exit([Other, State])
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
get_opinions_sync() -> % for debug
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {?CSV_URL, []}, [{timeout, 5000}], [{sync, true}]),
    Body.

make_responses_(Opinions, Txt) ->
    lists:foldl(
            fun({Regex, Res} = Op, {AccRes,AccOps} = Acc) -> 
                case re:run(Txt, Regex) of
                    {match, [_,_,{B, E}|_]} = M->
                        io:format("matched ~p~n",[{M, Txt, Res}]),
                        {[string:substr(Txt, B+1, E) ++ "? " ++ Res|AccRes], [Op|AccOps]};
                    nomatch ->
                        Acc
                end
            end, {[],[]}, Opinions).

parse_opinions(Txt) -> 
    ecsv:process_csv_string_with(Txt, 
            fun(Line,Acc) ->
                case Line of 
                    {newline, [Regex|Res]} ->
                        {ok, R} = re:compile("(\\(|\\s|^)(" ++ Regex ++ ")(\\s|\\)|\\.|\\?|\\!|$)", [unicode, caseless]),
                        L = {R, string:join(lists:filter(fun(X) -> X /= "" end, Res), ", ")},
                        [L|Acc];
                    _ ->
                        Acc
                end
            end).

start_app(App) ->
    case application:start(App) of
        ok -> ok;
        { error, already_started} -> ok;
        E -> E
    end.
