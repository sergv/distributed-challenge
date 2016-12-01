# Total order broadcast challenge #

## Bulid environment

The project was tested with GHC 7.10.3.

## Communication strategy

Nodes continuously broadcast messages with random numbers to each
other. However, every message is required to be acknowledged at least
by one process, otherwise it will be re-sent until sender get’s an
acknowledgement. This way it can be guaranteed that message was not
lost due to some network failure.

If some node doesn’t communicate with current node for extended period
of time, i.e. if message count received from it is less that maximum
number of messages sent by any node then current node will request
it’s peers to share their communication state with the faulty node in
order to catch up. Synchronization will happen on it’s own timeframe,
in order to not hinder sending of messages by other processes.

However, this strategy relies on the fact that processes don’t crash
randomly, otherwise message sent by process `A` to everyone but received
only by process `B` can be lost if `B` crashes. I assume that crashes are
not possible, i.e. I consider only well-behaved processes that don’t
crash but may experience arbitrary network failures that will
eventually go away. If at least one well-behaved process gets the
message then it will be able to share it with other processes upon
request, provided it won’t be permanently cut out of the network.

The reliability can be improved by requiring `N` acknowledgements for
each message sent. If less than `N` acknowledgements were received the
same message would be resent again. I’m not sure whether it’s
worthwhile to use `N` greater than 1 in this challenge and which
particular amount to require, so I leave it at 1 in order to sacrifice
reliability for the raw speed since high score seems to be more
important for now.

Messages get assigned a global order depending on message id, wich
get’s incremented by a sender process once acknowledgement was
received and place of sender process within global set of nodes.

## Configuring nodes

File `nodes.txt` should contain list of `NodeId`s, one per line. E.g.

```
10.10.10.2:7000:0
10.10.10.2:7001:0
10.10.10.2:7002:0
10.10.10.3:7003:0
```

## General notes

I’m using TCP in order to get delivery-in-order guarantees. I think it
may be possible to use UDP for greater flexibility and throughput, but
I haven’t tried that.

## Limitations

It is not clear to me how frequently each node should broadcast
messages and synchronize state with other nodes. The frequency seems
to depend on the overall node count - if there are many nodes and
all of them are sending messages frequently then it will resutl in
DDOS-like situation for the underlying network, which will start to
behave funny.

Time to synchronize with other peers also depends on the overall
length of the execution. I.e. if executing for as short time the
synchronization should also be short, otherwise it will happen too
rarely to help fix any problems.

Since I don’t know the exact number of nodes this will be tested on,
nor it is possible to optimize for all numbers of nodes beforehand,
I’d like to point out two command-line parameters that control intervals
between sending messages:

`--tick-interval` determines how frequently nodes send messages to
each otehr.  And `--peer-synchronization-interval` determines how
frequently nodes request their peers to share state in order to get
updated view on the messages sent.

Default for `--tick-interval` is 0.1 seconds, which seems to work ok for
node counts up to 50 on local machine. However it’s too big if there are
less than 10 nodes and probably too small if there are more than 50 nodes.

Default for `--peer-synchronization-interval` is 2 second with similar
caveats as for `--tick-interval`.

## Debugging

Debug messages can be enabled with `--debug-messages=on` command-line
flag.

## Technical notes on Haskell techniques used

### Error handling

I’m using `error` in order to indicate violation of invariants. However, this is
less than satisfactory practice. The proper solution would be to use `MonadError`
typeclass together with `distributed-process-lifted` package.

### Logging

It would be proper to use `Reader` monad to control whether logging is
enabled or disabled, but I chose simpler approach with implicit
parameter that configures logging.
