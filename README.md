# Erlang Gotchas

This is intended to be a compilation of various things related to the Erlang
runtime that can severely impact performance of an Erlang application if you
don't know about them and consider them in the design of your Erlang applications.
This is not intended to be comprehensive but it consists of the things that
I have learned the hard way. Hopefully it can help you not repeat my mistakes.

## Selective receive

### Background

To understand the gotcha related to selective receive, you have to understand
a bit about how Erlang works. An Erlang application consists of one or more
processes which do work generally in response to handling messages. Each process
is a very light-weight "green thread" rather than OS threads so there is very
little overhead. An application can easily consist of hundreds of thousands
of processes and the Erlang runtime will do a great job of scheduling them
all fairly when they have work to do.

An Erlang process is simple in that it primarily consists of just a call stack
and a message queue. A typical Erlang process waits for a message to arrive
in its message queue and it processes it and then waits for another message.
The message queue is generally a FIFO where the first message to enter the
queue is processed before other messages.

Messages are received by using the `receive` keyword. Multiple different message
patterns can be specified in the same `receive` statement such that the first
message in the queue matching one of the patterns is the one that is received.
If one of the pattens is just a single variable then it will match any message
and the message received will always be the first message in the queue. Here's
an example:

```
receive
    foo -> io:format("I received the atom foo~n");
    {foo, bar} -> io:format("I received the tuple {foo, bar}~n");
    Any -> io:format("I received the message ~p~n", [Any])
end
```

In this example, if the first message in the queue is the atom `foo` then it
will be handled by the first pattern match statement. If the first message in
the queue is the tuple `{foo, bar}` then it will be handled by the second
pattern match statement. If the first message in the queue is any other message,
it will be handled by the third pattern match statement. The third pattern match
is effectively a wildcard match. If there are no messages in the queue then the
process will be suspended until a message arrives matching one of the statements.

One feature of Erlang is that it is possible for messages to be handled out of
order by not specifying a wildcard pattern in a receive statement. For example:

```
receive
    {foo, _Foo} -> io:format("I received a tuple starting with foo~n");
    {bar, _Bar} -> io:format("I received a tuple starting with bar~n")
end
```

In this example, each message in the queue will be checked to see if it matches
either of the two patterns and if it does, it will be removed from the queue
and received by the process before the messages that were ahead of it in the
queue. If no messages in the queue match either of the two patterns then each
message that arrives in the queue will be matched against the two patterns and
handled immediately if it matches or else placed at the end of the queue if not.
This feature of Erlang is called selective receive.

This is a useful feature. It is this feature that allows for synchronous operations.
Normally, an Erlang process communicates with other processes (as well as native
processes) by sending a message which generally is done asynchronously. This
means that if a response is expected, the process needs to wait for it to arrive
in its message queue. If a process wants to receive the response before continuing,
it needs to use selective receive to match a specific message pattern that the
response is expected have.

### The problem

Now you should have enough background to understand the explanation as to why
selective receive can be a problem. If a process does very little work then
it may never get behind processing messages. However, if selective receive is
used by by a very busy process then the message queue may not be empty at the
point when it is used. Remember that the implementation of `receive` scans
all messages in the queue front to back.

Think about that. Scanning the message queue is an O(N) operation where N is
the number of messages in the queue. Handling N messages is also an O(N)
operation. If handling each message in the queue requires a selective receive
operation then handling N messages becomes an O(N^2) operation. See the problem?

In case you're skeptical, let's try it and see. You can see for yourself by
running a script which is here in this project. At the command line run the
following commands and you should see output similar to the following:

```
$ make
$ ./selective_receive.escript 1000
1000 selective receive loops took 0.002 seconds
```

Now try doubling the number of loops and see what happens. This is what
happens when I do it:

```
$ ./selective_receive.escript 2000
2000 selective receive loops took 0.011 seconds
$ ./selective_receive.escript 4000
4000 selective receive loops took 0.043 seconds
$ ./selective_receive.escript 8000
8000 selective receive loops took 0.169 seconds
$ ./selective_receive.escript 16000
16000 selective receive loops took 0.685 seconds
$ ./selective_receive.escript 32000
32000 selective receive loops took 2.736 seconds
```

Each time I have doubled the number of receive loops procesed and each time
the amount of time it takes to process them increases by a factor of 4. If
it were a linear increase, the time taken should increase by a factor of 2,
not 4. An increase by a factor of 4 is exactly what we would expect for an
O(N^2) algorithm.

Let's look briefly at the code to see what it's doing.

The `selective_receive.escript` script just converts the command line argument
to an integer and calls the `selective_receive/1` function in `gotchas.erl` so
I won't cover that. Here is the implementation of `selective_receive/1`:

```Erlang
selective_receive(Count) ->
    Pid = erlang:spawn(fun() -> selective_receive_entry(Count) end),
    lists:foreach(fun(I) ->
            erlang:send(Pid, {queued_message, I})
        end, lists:seq(1, Count)),
    erlang:send(Pid, {start, erlang:self()}),
    receive
        {done, Duration} -> Duration
    end.
```

What this does is it spawns a second process, sends `Count` messages to it
in the form of `{queued_message, I}` where `I` is an integer produced by the
`lists:seq/2` function which just generates a list of sequential integers.
After doing that then it sends a message to the child process to start
which includes the process ID of the parent process so that the child process
will be able to respond to the parent process when it has finished. The parent
process then waits for a message matching `{done, Duration}` and it returns
the `Duration` to the `selective_receive.escript` which prints the time it
took for the child process to finish.

The entry point for the child process is the `selective_receipe_entry/1` function.
Let's look at that:

```Erlang
selective_receive_entry(Count) ->
    receive
        {start, Parent} ->
            Duration = time_fun(fun() -> selective_receive_loop(Count) end),
            erlang:send(Parent, {done, Duration})
    end.
```

The child process' entry function will wait for the parent to send it the
start message before beginning to process any of the messages already in its
queue. This is done so that the two processes are not competing for resources
and we are measuring only the time taken to process `Count` messages. Remember
that the parent process will send the start message after sending `Count`
messages to the child process.

Let's look now at the `selective_receive_loop/1` function which is what
does all the work:

```Erlang
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
```

Notice first that the loop termination condition is when the `Count` parameter
is 0. If `Count` is not 0, the loop function will first send a message to itself
which is the atom `full_queue_scan`. There is nothing special about this message
except that it will cause a full queue scan because it will be placed at the end
of the queue behind all of the `{queued_message, I}` messages. Immediately after
sending this message, the child process will perform a selective receive
matching the `full_queue_scan` pattern. Then it will receive a single message
matching `{queued_message, I}` and call itself, passing `Count - 1`. In case
you're not familiar, this is utilizing another feature of Erlang where tail
recursion is optimized such that if a function call does not have any more
work to do in the current function when a function call is made, the called
function replaces the current one in the call stack. Otherwise we would have
to worry about a call stack growing without bounds. The effect here is that
the loop will be performed `Count` times. After that, the `selective_receive_loop`
function will return to the `selective_receive_entry` function which will
send the time taken to the parent process. The measurement is done with a simple
function called `time_fun` which I won't present here but you can see it in the
source if you're curious how it works.

So now it should be clear that selective receive can have a serious performance
penalty if used improperly. Why does this matter? Shouldn't it be easy to just
not use this programming pattern in Erlang? Not necessarily.

First, it is the way all calls to `gen_server:call/2` are handled. `gen_server:call/2`
will send a message to the target process and then use selective receive to either
wait for a response or receive a message indicating that the target process is
dead. Since most Erlang processes are `gen_server` processes, it is difficult to
avoid interacting with one or designing an application that doesn't depend on
one. Other common operations that involve selective receive are ones like calls
to `supervisor:start_child/2`. They can even be much more subtle like calls to
`gen_udp:send/4` which sends a message to an Erlang port (not a UDP port) and
then uses selective receive to receive a message from the port indicating whether
the operation succeeded or failed.

### The solution

There is a solution to this problem. To understand why the solution works, it
is necessary to talk a bit more about the problem, though. A busy process that
is handling a large volume of messages will often receive messages at a steady
interval when averaged over time but the rate will be noisy. There will be spikes
and lulls. If having a backlog of messages to process were free (and we've shown
that it is not) then as long as the average rate of messages received were below
the capacity of the process to handle them, the process should be able to keep
up. However, if a momentary increase in the rate causes the process to accumulate
a backlog of messages in its queue, then the cost of processing each message
increases if the handling of messages involves selective receive. Thus, a
temporary condition of the process lacking capacity to handle the rate of messages
it is receiving can easily become a permanent one. Worse, the further behind
the process gets, the slower it will handle each message. The situation may
quicly digress to the point that the message queue of this process is growing
fast enough that the Erlang process will run out of memory or cause swapping
which then, of course, affects the entire node.

The solution is to utilize a strategy such that before any message is handled,
the entire message queue is drained into process memory. That way, if a handling
the message involves selective receive, the queue of messages to be scanned
only includes those messages that have arrived in it while handling the current
message. I will show separately why this strategy doesn't entirely avoid the
problem, but this strategy is certainly better than allowing a large message
queue to be scanned repeatedly.

But this sounds like a lot of work to implement this solution, doesn't it?
It turns out it is already implemented for you, though not by the standard
Erlang OTP library. Instead, you have the fine folks who developed [RabbitMQ](https://www.rabbitmq.com/)
who have put together a `gen_server` compatible module called `gen_server2`
which handles draining the process message queue into memory before dispatching
a message to its handler.

### System processes

Unfortunately, there are lots of system processes that could potentially get
temporarily overwhelmeed and which may rely on selective receive and which
are written using `gen_server` rather than `gen_server2`. The one I have most
often observed to suffer from the selective receive problem is the system
`error_logger` process. If a problem occurs in your application that causes
a great deal of logging, you may see that the `error_logger` has gotten way
behind. A symptom of this problem would be that you continue to see error
messages being logged long after the problem that caused them has been resolved.
This could be misleading, in fact, and might make you think that a problem is
still occurring when it is not. You might not be able to tell that this is
what's happening unless you are regularly measuring and reporting the queue
lengths of all processes. You can inspect the queue lengths of processes
using the `erlang:process_info/1` or `erlang:process_info/2` functions.
Consider adding a process to your application that periodically checks the
length of the message queue of all processes and reports any that have very
large queues.
