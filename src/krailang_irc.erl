-module(krailang_irc).
-behaviour(gen_server).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {parent, sock, chan, timer, args}). % args is [Parent, Server, Port, User, Chan]

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([Parent, _Server, _Port, _User, Chan] = Args) -> 
    io:format("starting irc ~p~n", [Args]),
    {ok, Sock} = connect(Args),
    {ok, Timer} = timer:apply_after(1000*60*5, gen_server, call, [self(), reconnect]),
    {ok, #state{parent=Parent, sock=Sock, timer=Timer, chan=list_to_binary(Chan), args=Args}}.

connect([_Parent, Server, Port, User, _Chan]) ->
    {ok, Sock} = gen_tcp:connect(Server, Port, [binary, {packet, line}]),
    send(Sock, user, User ++ " * *", "lul"),
    send(Sock, nick, User),
    %% chan in binary for matching PRIVMSG
    {ok, Sock}.

handle_call(reconnect, _From, #state{sock=OldSock, args=Args} = State) ->
    gen_tcp:close(OldSock),
    {ok, Sock} = connect(Args),
    {ok, State#state{sock=Sock}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({irc, say, Txt}, #state{sock=Sock, chan=Chan} = State) ->
    send(Sock, privmsg, Chan, binary:replace(list_to_binary(Txt), [<<"\r">>,<<"\n">>],<<" ">>, [global])),
    {noreply, State};
handle_info({tcp, Sock, Data}, #state{parent=Parent, sock=Sock, chan=Chan, timer=Timer} = State) ->
%   io:format("got ~p~n", [Data]),
    Line = parse_line(Data),
%   io:format("parsed ~p~n", [Line]),
    NewState = case Line of  
        {<<"PING">>, _, Token, _, _} ->
            send(Sock, pong, "", Token),
            {ok, cancel} = timer:cancel(Timer),
            {ok, NewTimer} = timer:apply_after(1000*60*5, gen_server, call, [self(), reconnect]),
            State#state{timer=NewTimer};
        {<<"376">>, _, _, _, _} ->
            send(Sock, join, Chan),
            State;
        {<<"PRIVMSG">>, [Chan], [Msg], User, _} ->
            Parent ! {irc, self(), binary_to_list(User), binary_to_list(Msg) },
            State;
        _ ->
            State
    end,
    {noreply, NewState};
handle_info(Other, State) ->
    io:format("weird ~p ~p ~n", [Other, State]),
    {stop, badarg, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

send(Sock, Command, Arg) ->
    ok = gen_tcp:send(Sock, list_to_binary([string:to_upper(atom_to_list(Command)), " ", Arg, "\r\n"])).
send(Sock, Command, Arg, Txt) ->
    ok = gen_tcp:send(Sock, list_to_binary([string:to_upper(atom_to_list(Command)), " ", Arg, " :", Txt, "\r\n"])).

parse_line(<<":", Txt/binary>>) ->
    TxtS = binary:replace(Txt, <<"\r\n">>,<<"">>),
    [Attr|Msg] = binary:split(TxtS, <<":">>),
    [User,Command|Args] = binary:split(Attr, <<" ">>, [global, trim]),
    [UserName|Host] = binary:split(User, <<"!">>),
    {Command, Args, Msg, UserName, Host};

parse_line(Txt) ->
    [Attr|Msg] = binary:split(Txt, <<":">>),
    [Command|Args] = binary:split(Attr, <<" ">>, [global, trim]),
    {Command, Args, Msg, <<"">>, <<"">>}.
