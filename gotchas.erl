-module(gotchas).

-export([selective_receive/1]).


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
