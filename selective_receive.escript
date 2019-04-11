#!/usr/bin/env escript

main([CountStr]) ->
    Count = erlang:list_to_integer(CountStr),
    Duration = gotchas:selective_receive(Count),
    io:format("~p selective receive loops took ~p seconds~n", [Count, Duration]);
main(_Args) ->
    io:format("usage: selective_receive.escript <count>~n", []).

