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

-record(state, {opinions}).
-define(CSV_URL, "http://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AoVJwKRm4drQdDdBY2hQWVBiSGtrMWsycGZzM0hKM3c&single=true&gid=0&output=csv"). 

%% interface

start_link() ->
    start_app(inets),
    start_app(crypto),
    start_app(public_key),
    start_app(ssl),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
make_responses(Pid, Txt) ->
    gen_server:call(Pid, {make_responses, Txt}).

%% gen_server callbacks

init([] = Args) ->
    io:format("starting bot~p~n", [Args]),
    get_opinions(),
    {ok, #state{opinions=[]}}.

handle_call({make_responses, Txt}, _From, #state{opinions=Opinions} = State) ->
    {reply, make_responses_(Opinions, Txt), State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of 
        {http, {_, {{_, 200, _}, _, Body}}} ->
            {ok, Opinions} = parse_opinions(binary_to_list(Body)), % for some reason, async returns bodies as binary
            {noreply, State#state{opinions=Opinions}};
        Other ->
            io:format("weird ~p ~p ~n", [Other, State]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
get_opinions() ->
    {ok, _} = httpc:request(get, {?CSV_URL, []}, [{timeout, 5000}], [{sync, false}]).
get_opinions_sync() -> % for debug
    httpc:request(get, {?CSV_URL, []}, [{timeout, 5000}], [{sync, true}]).

make_responses_(Opinions, Txt) ->
    lists:foldl(
            fun({Regex, Res}, Acc) -> 
                case re:run(Txt, Regex) of
                    {match, [_,_,{B, E},_]} ->
                        [string:substr(Txt, B+1, E) ++ "? " ++ Res|Acc]; % substr is 1-based, run is 0-based 
                    nomatch ->
                        Acc
                end
            end, [], Opinions).

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
