

# Module usagi #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ack-2">ack/2</a></td><td></td></tr><tr><td valign="top"><a href="#ack-3">ack/3</a></td><td></td></tr><tr><td valign="top"><a href="#bind_exchange-4">bind_exchange/4</a></td><td></td></tr><tr><td valign="top"><a href="#bind_queue-4">bind_queue/4</a></td><td></td></tr><tr><td valign="top"><a href="#cancel_consume-2">cancel_consume/2</a></td><td></td></tr><tr><td valign="top"><a href="#close_channel-1">close_channel/1</a></td><td></td></tr><tr><td valign="top"><a href="#consume_queue-3">consume_queue/3</a></td><td></td></tr><tr><td valign="top"><a href="#consume_queue-4">consume_queue/4</a></td><td></td></tr><tr><td valign="top"><a href="#consumer_count-2">consumer_count/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_exchange-2">delete_exchange/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_queue-2">delete_queue/2</a></td><td></td></tr><tr><td valign="top"><a href="#discard-2">discard/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg-0">get_msg/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg-2">get_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg-3">get_msg/3</a></td><td></td></tr><tr><td valign="top"><a href="#msg_count-2">msg_count/2</a></td><td></td></tr><tr><td valign="top"><a href="#new_channel-1">new_channel/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_channel-2">new_channel/2</a></td><td></td></tr><tr><td valign="top"><a href="#publish-4">publish/4</a></td><td></td></tr><tr><td valign="top"><a href="#reject-2">reject/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>start usagi application.</td></tr><tr><td valign="top"><a href="#start_exchange-3">start_exchange/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_queue-2">start_queue/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>stop usagi application.</td></tr><tr><td valign="top"><a href="#unpack-1">unpack/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ack-2"></a>

### ack/2 ###

`ack(Channel, Tag) -> any()`


<a name="ack-3"></a>

### ack/3 ###

`ack(Channel, Tag, Multi) -> any()`


<a name="bind_exchange-4"></a>

### bind_exchange/4 ###

`bind_exchange(Channel, Source, Destination, Key) -> any()`


<a name="bind_queue-4"></a>

### bind_queue/4 ###

`bind_queue(Channel, Exchange, Queue, Key) -> any()`


<a name="cancel_consume-2"></a>

### cancel_consume/2 ###

`cancel_consume(Channel, Tag) -> any()`


<a name="close_channel-1"></a>

### close_channel/1 ###

`close_channel(Channel) -> any()`


<a name="consume_queue-3"></a>

### consume_queue/3 ###

`consume_queue(Channel, Queue, Receiver) -> any()`


<a name="consume_queue-4"></a>

### consume_queue/4 ###

`consume_queue(Channel, Queue, Receiver, NoAck) -> any()`


<a name="consumer_count-2"></a>

### consumer_count/2 ###

`consumer_count(Channel, Queue) -> any()`


<a name="delete_exchange-2"></a>

### delete_exchange/2 ###

`delete_exchange(Channel, Name) -> any()`


<a name="delete_queue-2"></a>

### delete_queue/2 ###

`delete_queue(Channel, Queue) -> any()`


<a name="discard-2"></a>

### discard/2 ###

`discard(Channel, Tag) -> any()`


<a name="get_msg-0"></a>

### get_msg/0 ###

`get_msg() -> any()`


<a name="get_msg-2"></a>

### get_msg/2 ###

`get_msg(Channel, Queue) -> any()`


<a name="get_msg-3"></a>

### get_msg/3 ###

`get_msg(Channel, Queue, NoAck) -> any()`


<a name="msg_count-2"></a>

### msg_count/2 ###

`msg_count(Channel, Queue) -> any()`


<a name="new_channel-1"></a>

### new_channel/1 ###

`new_channel(Name) -> any()`


<a name="new_channel-2"></a>

### new_channel/2 ###

`new_channel(Rabbit, Name) -> any()`


<a name="publish-4"></a>

### publish/4 ###

`publish(Channel, Exchange, Key, Event) -> any()`


<a name="reject-2"></a>

### reject/2 ###

`reject(Channel, Tag) -> any()`


<a name="start-0"></a>

### start/0 ###


<pre><code>
start() -&gt; ok
</code></pre>


start usagi application.
<a name="start_exchange-3"></a>

### start_exchange/3 ###

`start_exchange(Channel, Name, Type) -> any()`


<a name="start_queue-2"></a>

### start_queue/2 ###

`start_queue(Channel, Queue) -> any()`


<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; ok
</code></pre>


stop usagi application.
<a name="unpack-1"></a>

### unpack/1 ###

`unpack(Basic.consume_ok) -> any()`


