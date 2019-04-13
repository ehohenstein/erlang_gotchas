-module(gotchas).

-export([selective_receive/1]).

-export([start_sink/0, start_source/2]).


% Selective Receive

selective_receive(Count) ->
    Pid = erlang:spawn(fun() -> selective_receive_entry(Count) end),
    lists:foreach(fun(I) ->
            erlang:send(Pid, {queued_message, I})
        end, lists:seq(1, Count)),
    erlang:send(Pid, {start, erlang:self()}),
    receive
        {done, Duration} -> Duration
    end.

selective_receive_entry(Count) ->
    receive
        {start, Parent} ->
            Duration = time_fun(fun() -> selective_receive_loop(Count) end),
            erlang:send(Parent, {done, Duration})
    end.

selective_receive_loop(0) ->
    ok;
selective_receive_loop(Count) ->
    erlang:send(erlang:self(), full_queue_scan),
    receive
        full_queue_scan -> ok
    end,
    receive
        {queued_message, _I} -> selective_receive_loop(Count - 1)
    end.

time_fun(Fun) ->
    Before = erlang:system_time(millisecond),
    Fun(),
    After = erlang:system_time(millisecond),
    erlang:float(After - Before) / 1000.0.

% Sending to an Unresponsive Node

start_sink() ->
    io:format("starting sink in pid ~p~n", [erlang:self()]),
    erlang:register(sink, erlang:self()),
    sink_loop().

sink_loop() ->
    receive
        stop ->
            ok;
        Any -> 
            report(io_lib:format("sink received message ~p", [Any])),
            sink_loop()
    end.

start_source(TargetNode, Flags) ->
    Sink = rpc:call(TargetNode, erlang, whereis, [sink]),
    io:format("found sink with pid ~p on node ~p~n", [Sink, TargetNode]),
    io:format("starting source using flags ~p~n", [Flags]),
    source_loop(Sink, Flags, 0).

source_loop(Sink, Flags, I) ->
    report(io_lib:format("sending message ~p to sink", [I])),
    case erlang:send(Sink, {foobar, I}, Flags) of
        Error when Error =:= nosuspend orelse Error =:= noconnect ->
            report(io_lib:format("sending message failed due to ~p", [Error]));
        _Other ->
            ok
    end,
    source_loop(Sink, Flags, I + 1).

report(Message) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io:format("~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w: ~s~n", [Year, Month, Day, Hour, Min, Sec, lists:flatten(Message)]).
