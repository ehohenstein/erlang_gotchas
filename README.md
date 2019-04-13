# Erlang Gotchas

This is intended to be a compilation of various things related to the Erlang
runtime that can severely impact performance of an Erlang application if you
aren't aware of them and consider them in the design of your Erlang applications.
This is not intended to be a comprehensive list but it consists of the things that
I have learned the hard way. Hopefully it can help you not repeat my mistakes.

## Selective Receive

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

```Erlang
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

```Erlang
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
erlc gotchas.erl
$ ./selective_receive.escript 1000
1000 selective receive loops took 0.002 seconds
```

Now try doubling the number of loops and see what happens. This is what
happens when I do it in an Ubuntu 18.04 VM using Erlang 21.3, though I have
observed similar results in many other versions of Erlang, at least as
far back as Erlang R15B02:

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
the amount of time it takes to process them increases by approximately a factor
of 4. If it were a linear increase, the time taken should increase by a factor
of 2, not 4. An increase by a factor of 4 is exactly what we would expect for an
O(N^2) algorithm when N is doubled.

Note that if you make N really really large, your system may start to swap since
the test involves creating N messages simultaneously in memory. You should attempt
to verify that you are not measuring swap. I can confirm that swap did not occur
while producing the output above.

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
function called `time_fun/1` which I won't present here but you can see it in the
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
the entire message queue is drained into process memory. That way, if handling
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
behind processing log messages. A symptom of this problem would be that you
continue to see error messages being logged long after the problem that caused
them has been resolved. This could be misleading, in fact, and might make you
think that a problem is still occurring when it is not. You might not be able
to tell that this is what's happening unless you are regularly measuring and
reporting the queue lengths of all processes. You can inspect the queue lengths
of processes using the `erlang:process_info/1` or `erlang:process_info/2`
functions. Consider adding a process to your application that periodically
checks the length of the message queue of all processes and reports any that
have very large queues.

## Sending to an Unresponsive Node

### Background

Erlang is known for having infrastructure for writing robust applications. It
is famous for having delivered nine nines of uptime in an early application
for which the language was developed by Ericcson.

You might assume that this robust application infrastructure of Erlang would
handle the case of a node in an Erlang cluster dying, right? Well, yes, it will
for certain kinds of node "death", and for others then, yes, it will, so long
as you know how to deal with them appropriately.

Let's first cover how an Erlang node might fail.

There is the obvious way an Erlang node might fail which is where the
application code might have an error and an Erlang process might crash.
Erlang has a system of supervisors for this purpose which will monitor
child processes, including other supervisors, and restart them if they exit
unexpectedly. This is not the kind of "death" I will discuss here.

There is the also obvious way where the Erlang runtime might crash, taking out
all the running Erlang processes with it. There is also infrastructure built
around Erlang called heartbeat which will restart a crashed node. This is also
not the kind of "death" I will discuss here.

The kind of death I am concerned with here is when an entire host where an Erlang
node is running vanishes from the network. By this, I mean that the host entirely
stops responding to network traffic.

There are a number of ways this precise problem can happen. All of them will
happen in production environments with some non-zero frequency, which is why
this is an important issue to address when discussing the robustness of an
Erlang application. Here is a non-exhaustive list of some of the ways this
problem can occur, though it perhaps includes all the scenarios that are most
likely:
* The host where an Erlang node is running has started heavily swapping and is
unable to schedule the network driver to send or receive traffic.
* The host where an Erlang node is running has had a kernel panic and has halted.
* The host where an Erlang node is running has lost power.
* The host where an Erlang node is running has had a hardware failure and has
halted.
* The host where an Erlang node is running has had a triple fault and has
spontaneously rebooted and is unreachable until its network services have
been restarted.
* A switch connecting nodes in an Erlang cluster fails resulting in a partitioned
network.

I've seen every one of these problems occur to an Erlang cluster in a production
environment and until I solved this problem, it caused the failure to cascade
to every node in the cluster resuling in all nodes essentially halting. The
reason is simple, but not obvious. I'll show you how it happens and how to
avoid it.

### Erlang Cluster Connection Details

To understand how Erlang behaves when a node is unresponsive, we need to
first talk about how nodes in an Erlang cluster communicate. You might know
that when an Erlang node connects to one node in an Erlang cluster, it is
automatically connected to every node in the cluster (this is not true if
`erl` is started with the `-connect_all false` flag). Internally, for every
node an Erlang node is connected to, an internal buffer is allocated to store
outgoing messages to that target node. The size of this buffer is configurable.
Here is an excerpt from the documentation of the `erl` program that describes
this configuration:

```
+zdbbl size
    Sets the distribution buffer busy limit (dist_buf_busy_limit) in kilobytes. Valid
    range is 1-2097151. Defaults to 1024.

    A larger buffer limit allows processes to buffer more outgoing messages
    over the distribution. When the buffer limit has been reached, sending
    processes will be suspended until the buffer size has shrunk. The buffer
    limit is per distribution channel. A higher limit gives lower latency and
    higher throughput at the expense of higher memory use.
```

The important thing to notice here is the text *When the buffer limit has been
reached, sending processes will be suspended until the buffer size has shrunk.*

It is well documented that sending Erlang messages is not guaranteed to succeed.
The reason is because it isn't even clear what a definition of "succeed" might
be. And for any definition, there is no way to make such a guarantee. Even if
the target process is still alive at the moment a message is sent, that doesn't
mean that it will be alive by the time it gets around to handling the message.
There is no way to even know that the target process will ever invoke a
`receive` operation that would match the message, even if it were to operate
indefinitely.

Why do these things matter? Because though sending messages almost never block
and messages are not guaranteed to be received, these things almost always work
as expected. As a result, Erlang programmers tend to develop their applications
by assuming that the sending of messages is both always asynchronous and always
successful. It turns out that when an Erlang node goes network silent, neither
of these things are true and the consequences can be disasterous.

### Demonstration

To illustrate, I've put together a demonstration. Unforunately, none of the
conditions I mentioned above that might cause this problem are easy to replicate.
However, there are ways to simulate the same thing. Unfortunately again, there
is still some sigificant setup required. This demonstration will require the
installation of `docker-engine` and we'll need to build some containers and use
three different shells. You will also need `sudo` access on your system. The
setup commands and all the supporting code referenced below are tested on an
Ubuntu 18.04 system. If you have something different, you may have to figure
out how to make the demonstration work on your system.

First, we need to install the `docker-engine` package:

```
sudo apt-get install docker-engine
```

Now if you want to be able to invoke docker commands without running sudo
every time:

```
sudo usermod -aG docker $USER
```

Then you'll need to exit your shell and start another one for that change to
take effect.

Now we want to build the containers that docker will run for the demo:

```
make containers
```

That will take a while since it will need to download and install erlang
packages in the containers being built. Once it's done, there's one more bit
of setup we want to do which is to create the bridge network for our demo:

```
make erl-net
```

Now that that's done, you will want to start a second and third shell.

In the second shell, run this command:

```
make start-sink
```

In the third shell, run this command:

```
make start-source
```

You should see a rapid stream of messages scrolling up in each of the second
and third shell windows. Each line of text is indicating a message sent by
and Erlang node running in a container in the third shell to another Erlang
node running in a container in the second shell.

The output in the second shell will look something like this:

```
2019-04-12 23:51:19: sink received message {foobar,14851}
2019-04-12 23:51:19: sink received message {foobar,14852}
2019-04-12 23:51:19: sink received message {foobar,14853}
2019-04-12 23:51:19: sink received message {foobar,14854}
2019-04-12 23:51:19: sink received message {foobar,14855}
2019-04-12 23:51:19: sink received message {foobar,14856}
2019-04-12 23:51:19: sink received message {foobar,14857}
2019-04-12 23:51:19: sink received message {foobar,14858}
2019-04-12 23:51:19: sink received message {foobar,14859}
```

And the output in the third shell will look something like this:

```
2019-04-12 23:51:21: sending message 28287 to sink
2019-04-12 23:51:21: sending message 28288 to sink
2019-04-12 23:51:21: sending message 28289 to sink
2019-04-12 23:51:21: sending message 28290 to sink
2019-04-12 23:51:21: sending message 28291 to sink
2019-04-12 23:51:21: sending message 28292 to sink
2019-04-12 23:51:21: sending message 28293 to sink
2019-04-12 23:51:21: sending message 28294 to sink
```

Back in the first shell, run this command:

```
make pause-sink
```

You should see the messages stop being printed in the second shell window
immediately and they should continue in the third window for several seconds
at which point they should also stop in the third window.

What happened? We paused the container where the Erlang node was running that
was receiving messages from the Erlang node in the third shell. Obviously,
the paused Erlang node could not continue receiving and printing messages.
The other node continued sending messages until its buffer to the paused node
was full. At that point the process sending the messages started being blocked.
This is, in fact, exactly the problem I want to show.

Ths story doesn't end here though. If you leave the sink process paused and
leave the source process running but getting nowhere, you will see it eventually
start to continue but very very slowly. After approximately one minute, you
should see something like this in the third shell window:

```
2019-04-12 23:51:21: sending message 28308 to sink
2019-04-12 23:51:21: sending message 28309 to sink
2019-04-12 23:51:21: sending message 28310 to sink

=ERROR REPORT==== 12-Apr-2019::23:52:25 ===
** Node sink@sinkhost not responding **
** Removing (timedout) connection **
2019-04-12 23:52:25: sending message 28311 to sink
2019-04-12 23:52:32: sending message 28312 to sink
2019-04-12 23:52:39: sending message 28313 to sink
2019-04-12 23:52:46: sending message 28314 to sink
2019-04-12 23:52:53: sending message 28315 to sink
2019-04-12 23:53:00: sending message 28316 to sink
```

After the sink node has been recognized as down, the sending process will
become unblocked but each message it tries to send to the down node causes
it to attempt to reconnect. The reconnect takes seven seconds to fail so
the sending process will be blocked for seven seconds at a time every seven
seconds. Not much better right?

### The Solution

How do we avoid this problem? Let's take a look very briefly at how messages
are typically sent in Erlang. Typically the `!` operator is used which is
equivalent to the `erlang:send/2` function. Here are equivalent ways of
doing the same thing:

```
Pid ! some_message,
erlang:send(Pid, some_message)
```

There is, however, another way to send messages which is `erlang:send/3`.
This function takes a list of flags as the third parameter. Calling
`erlang:send/2` is equivalent to calling `erlang:send/3` with an empty
list of flags except that `erlang:send/2` returns the message that was
sent (or not sent) and `erlang:send/3` returns a value that indicates
whether it succeeded or failed.

There are two flags available to `erlang:send/3` which are `nosuspend`
and `noconnect`.

Unless the `noconnect` flag is used when sending a message in Erlang, then
if the node where the target process is running is not connected to the
local node, the local node will attempt to connect to that node.

Unless the `nosuspend` flag is used when sending a message in Erlang, then
if the buffer is full to the target node, the calling process will be
suspended.

When we rant the source node using the `make start-source` command, we
started it in a way such that it will use empty flags when it sends
messages to the sink node. The way this is done is by not including
the `SOURCE_FLAGS` environment variable when starting the source node
container so that the statement `[$SOURCE_FLAGS]` evaluated to start
the source process evaluated to `[]` in the shell. The messages,
therefore, are being sent like:

```
erlang:send(Sink, {foobar, I}, [])
```

This means that the source process will be able to continue sending
messages to the sink process after the sink container was paused until
the buffer to the sink node is full at which point the source process
is suspended until the source node has decided that the sink node is
down. After that the source process is waiting for a connect attempt
to fail every time it sends a message. This matches exactly what we
have observed.

Resume the sink node by running this in the first shell:

```
make resume-sink
```

You should see that the source immediately starts sending messages
to the sink and the sink immediately starts receiving them. Use
Ctrl-C in the third shell window to stop the source.

Let's try using the `nosuspend` flag when sending messages from the
source to the sink. Run this command in the third shell window:

```
make start-source-nosuspend
```

Again you should see the lots of messages scroll by being sent by the
source and received by the sink. Let's try pausing the sink again
in the first shell:

```
make pause-sink
```

After a few seconds, the sink will start to fail to send the messages
to the sink, reporting messages like these:

```
2019-04-13 04:25:05: sending message 204734 to sink
2019-04-13 04:25:05: sending message failed due to nosuspend
2019-04-13 04:25:05: sending message 204735 to sink
2019-04-13 04:25:05: sending message failed due to nosuspend
2019-04-13 04:25:05: sending message 204736 to sink
2019-04-13 04:25:05: sending message failed due to nosuspend
2019-04-13 04:25:05: sending message 204737 to sink
2019-04-13 04:25:05: sending message failed due to nosuspend
2019-04-13 04:25:05: sending message 204738 to sink
2019-04-13 04:25:05: sending message failed due to nosuspend
```

That's good. The source did not get blocked by the buffer being full
when the sink was not responding. In this case the source is sending
messages to the sink like this:

```
erlang:send(Sink, {foobar, I}, [nosuspend])
```

The sink is even able to report that the send failed. Failing is not
good, but if you're going to fail, it's better to fail fast, as the
source is doing. However, after approximately one minute, the source node
will decide that the sink node is down and it will start trying to connect
to it with each message sent. The source process is again blocked for
7 seconds every 7 seconds. It is failing slowly again.

Resume the sink again in the first shell:

```
make resume-sink
```

You should see the messages flowing again on both sides. As soon as the
sink was available again, the source connected to it and both source and
sink continued. Stop the source again with Ctrl-C in the third shell
window.

Now let's see what happens in this scenario when we use the `noconnect`
flag. Start the source again with this command:

```
make start-source-noconnect
```

You should see the messages flow from source to sink again. Now suspend
the sink in the first shell again:

```
make pause-sink
```

After a few seconds, the source process will halt, just like it did when
we passed no flags. After approximately one minute the source node will
decide that the sink is down and the source process will continue,
showing that it is failing to send messages to the sink due to `noconnect`.
After the buffer to the sink node filled up, the source process was blocked
when trying to send to the sink. Once the source node decided that the
sink node was down, it stopped blocking the source process and it did
not block additional attempts to send to the sink because `noconnect`
prevents each send operation from blocking while the node attempts to
connect to the target node.

For about a minute, the source process is not failing fast but then it
does fail fast. That's perhaps a bit better than when we used `nosuspend`
because the `nosuspend` option will fail fast for at most one minute but
the `noconnect` option will fail fast for potentially much longer. We
still haven't solved the problem though.

One thing to note there is that the source node will not automatically
reconnect to the sink node when the sink node resumes. Try doing that now
in the first shell:

```
make resume-sink
```

The sink may start to print some messages that it had received prior to
being paused but then it will stop. The source is continuing to fail to
send messages to the sink and the sink is not receiving the messages.
This state will continue until we take some action to resolve it.

Stop the source again in with Ctrl-C in the third shell.

Let's combine the flags and see what happens. In the third shell, start
the source node with this command:

```
make start-source-failfast
```

You should see messages again flowing between the source and sink nodes.
Suspend the sink node again in the first shell:

```
make pause-sink
```

After a few seconds, you should see that the sink is failing to send messages
to the source because of `nosuspend`. The buffer to the sink host is full.
After about a minute, the source node will decide that the sink node is
down and the failure reason will switch to `noconnect`.

Resume the sink node again in the first shell:

```
make resume-sink
```

Again, the sink will print some messages that it received before it was
paused but hadn't yet processed and again, the two nodes do not automatically
reconnect.

The good news is that we managed to prevent the source from ever blocking.
We made it continuously fail fast.

You can stop both the source and sink nodes with Ctrl-C in both the second
and third shell windows.

You might want to remove the bridge network that we created earlier with
this command:

```
make docker-clean
```

We managed to get Erlang to fail fast when a node in an Erlang cluster
becomes unresponsive. Failing fast is good. However, we're left with
the problem of needing to have some other mechanism to reconnect nodes
once a failure occurs. There are wide variety of ways to get this to happen
and I won't get into the details of how to do that. That is left as
an exercise for the reader.
