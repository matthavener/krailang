-module(krailang).
-compile(export_all).

client_loop(Sock, Parent) ->
    receive 
        Other -> 
            io:format("got ~p~n", [Other]),
            client_loop(Sock, Parent)
    end.

client(Server, Port, Parent) ->
    {ok, Sock} = gen_tcp:connect(Server, Port, [binary]),
    client_loop(Sock, Parent).

start_client(Server, Port) -> 
    spawn_link(
            ?MODULE,
            client,
            [Server, Port, self()]).


