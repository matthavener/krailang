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

-record(state, {parent, sock}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([Parent, Server, Port, User] = Args) ->
    io:format("starting~p~n", [Args]),
    {ok, Sock} = gen_tcp:connect(Server, Port, [binary, {packet, line}]),
    self() ! {irc, user, User ++ " * *", "wootles"},
    self() ! {irc, nick, User },
    {ok, #state{parent=Parent, sock=Sock}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, #state{parent=Parent, sock=Sock} = State) ->
    case Info of 
        {irc, privmsg, Channel, Txt} ->
            gen_tcp:send(Sock, build_line(privmsg, Channel, Txt));
        {irc, user, Username, Txt} ->
            io:format("send ~p~n", [build_line(user, Username, Txt)]),
            gen_tcp:send(Sock, build_line(user, Username, Txt));
        {irc, join, Chan} ->
            io:format("send ~p~n", [build_line(user, Chan)]),
            gen_tcp:send(Sock, build_line(user, Chan));
        {irc, nick, Username} ->
            io:format("send ~p~n", [build_line(nick, Username)]),
            gen_tcp:send(Sock, build_line(nick, Username));
        {tcp, Sock, Data} -> 
%            io:format("got ~p~n", [Data]),
            Line = parse_line(Data),
%            io:format("parsed ~p~n", [Line]),
            case Line of  
                {<<"PING">>, _, Token, _, _} ->
                    gen_tcp:send(Sock, build_line(pong, "", Token));
                Other ->
                    Parent ! { self(), Other }
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

build_line(Command, Arg) ->
    list_to_binary([string:to_upper(atom_to_list(Command)), " ", Arg, "\r\n"]).
build_line(Command, Arg, Txt) ->
    list_to_binary([string:to_upper(atom_to_list(Command)), " ", Arg, " :", Txt, "\r\n"]).

parse_line(<<":", Txt/binary>>) ->
    [Attr|Msg] = binary:split(Txt, <<":">>),
    [User,Command|Args] = binary:split(Attr, <<" ">>, [global, trim]),
    [UserName|Host] = binary:split(User, <<"!">>),
    {Command, Args, Msg, UserName, Host};

parse_line(Txt) ->
    [Attr|Msg] = binary:split(Txt, <<":">>),
    [Command|Args] = binary:split(Attr, <<" ">>, [global, trim]),
    {Command, Args, Msg, <<"">>, <<"">>}.
