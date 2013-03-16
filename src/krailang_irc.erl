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

-record(state, {parent, sock, chan}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([Parent, Server, Port, User, Chan] = Args) ->
    io:format("starting irc ~p~n", [Args]),
    {ok, Sock} = gen_tcp:connect(Server, Port, [binary, {packet, line}]),
    send(Sock, user, User ++ " * *", "lul"),
    send(Sock, nick, User),
    %% chan in binary for matching PRIVMSG
    {ok, #state{parent=Parent, sock=Sock, chan=list_to_binary(Chan)}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, #state{parent=Parent, sock=Sock, chan=Chan} = State) ->
    case Info of 
        {irc, say, Txt} ->
            send(Sock, privmsg, Chan, binary:replace(list_to_binary(Txt), [<<"\r">>,<<"\n">>],<<" ">>, [global]));
        {tcp, Sock, Data} -> 
%            io:format("got ~p~n", [Data]),
            Line = parse_line(Data),
%            io:format("parsed ~p~n", [Line]),
            case Line of  
                {<<"PING">>, _, Token, _, _} ->
                    send(Sock, pong, "", Token);
                {<<"376">>, _, _, _, _} ->
                    send(Sock, join, Chan);
                {<<"PRIVMSG">>, [Chan], [Msg], User, _} ->
                    Parent ! {irc, self(), binary_to_list(User), binary_to_list(Msg) };
                _ ->
                    ok
            end;
        Other ->
            io:format("weird ~p ~p ~n", [Other, State]),
            exit([Other, State])
    end,
    {noreply, State}.

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
