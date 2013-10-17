

# Module usagi #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ack-2">ack/2</a></td><td></td></tr><tr><td valign="top"><a href="#ack-3">ack/3</a></td><td>Ack messages by delivery tag.</td></tr><tr><td valign="top"><a href="#bind_exchange-4">bind_exchange/4</a></td><td>bind <code>Source</code> exchange to <code>Destination</code> exchange.</td></tr><tr><td valign="top"><a href="#bind_queue-4">bind_queue/4</a></td><td>bind <code>Queue</code> to <code>Exchange</code> with <code>Key</code> as routing key.</td></tr><tr><td valign="top"><a href="#cancel_consume-2">cancel_consume/2</a></td><td>Cancel consuming with <code>Tag</code></td></tr><tr><td valign="top"><a href="#close_channel-1">close_channel/1</a></td><td>Close a channel.</td></tr><tr><td valign="top"><a href="#consume_queue-3">consume_queue/3</a></td><td>Set <code>Receiver</code> as a consumer to <code>Queue</code></td></tr><tr><td valign="top"><a href="#consume_queue-4">consume_queue/4</a></td><td>Set <code>Receiver</code> as a consumer to <code>Queue</code>, also set <code>noack</code> option.</td></tr><tr><td valign="top"><a href="#delete_exchange-2">delete_exchange/2</a></td><td>delete an queue.</td></tr><tr><td valign="top"><a href="#delete_queue-2">delete_queue/2</a></td><td>delete a queue.</td></tr><tr><td valign="top"><a href="#discard-2">discard/2</a></td><td>Discard message by delivery tag.</td></tr><tr><td valign="top"><a href="#get_msg-0">get_msg/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg-2">get_msg/2</a></td><td>Get messges from queue.</td></tr><tr><td valign="top"><a href="#get_msg-3">get_msg/3</a></td><td>Get messges from queue, optionally setting noack option.</td></tr><tr><td valign="top"><a href="#new_channel-1">new_channel/1</a></td><td>start a new channel on default rabbit connection.</td></tr><tr><td valign="top"><a href="#new_channel-2">new_channel/2</a></td><td>start a new channel on a rabbit connection.</td></tr><tr><td valign="top"><a href="#publish-4">publish/4</a></td><td>publish a message.</td></tr><tr><td valign="top"><a href="#reject-2">reject/2</a></td><td>Reject message by delivery tag.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>start usagi application.</td></tr><tr><td valign="top"><a href="#start_exchange-3">start_exchange/3</a></td><td>start an exchange.</td></tr><tr><td valign="top"><a href="#start_queue-2">start_queue/2</a></td><td>start a queue.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>stop usagi application.</td></tr><tr><td valign="top"><a href="#unpack-1">unpack/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ack-2"></a>

### ack/2 ###


<pre><code>
ack(Channel :: <a href="#type-channel">channel()</a>, Tag :: binary()) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


__See also:__ [ack/3](#ack-3).
<a name="ack-3"></a>

### ack/3 ###


<pre><code>
ack(Channel :: <a href="#type-channel">channel()</a>, Tag :: binary(), Multi :: boolean()) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


Ack messages by delivery tag
<a name="bind_exchange-4"></a>

### bind_exchange/4 ###


<pre><code>
bind_exchange(Channel :: <a href="#type-channel">channel()</a>,Source :: <a href="#type-exchange">exchange()</a>,Destination :: <a href="#type-exchange">exchange()</a>,Key :: <a href="#type-routing_key">routing_key()</a>) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


bind `Source` exchange to `Destination` exchange
<a name="bind_queue-4"></a>

### bind_queue/4 ###


<pre><code>
bind_queue(Channel :: <a href="#type-channel">channel()</a>,Exchange :: <a href="#type-exchange">exchange()</a>,Queue :: <a href="#type-exchange">exchange()</a>,Key :: <a href="#type-routing_key">routing_key()</a>) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


bind `Queue` to `Exchange` with `Key` as routing key
<a name="cancel_consume-2"></a>

### cancel_consume/2 ###


<pre><code>
cancel_consume(Channel :: <a href="#type-channel">channel()</a>, Tag :: binary()) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


Cancel consuming with `Tag`
<a name="close_channel-1"></a>

### close_channel/1 ###


<pre><code>
close_channel(Channel :: <a href="#type-channel">channel()</a>) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


Close a channel
<a name="consume_queue-3"></a>

### consume_queue/3 ###


<pre><code>
consume_queue(Channel :: <a href="#type-channel">channel()</a>,Queue :: <a href="#type-r_queue">r_queue()</a>,Receiver :: pid()) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


Set `Receiver` as a consumer to `Queue`
<a name="consume_queue-4"></a>

### consume_queue/4 ###


<pre><code>
consume_queue(Channel :: <a href="#type-channel">channel()</a>,Queue :: <a href="#type-r_queue">r_queue()</a>,Receiver :: pid(),NoAck :: boolean()) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


Set `Receiver` as a consumer to `Queue`, also set `noack` option
<a name="delete_exchange-2"></a>

### delete_exchange/2 ###


<pre><code>
delete_exchange(Channel :: <a href="#type-channel">channel()</a>, Name :: <a href="#type-exchange">exchange()</a>) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


delete an queue
<a name="delete_queue-2"></a>

### delete_queue/2 ###


<pre><code>
delete_queue(Channel :: <a href="#type-channel">channel()</a>, Queue :: <a href="#type-r_queue">r_queue()</a>) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


delete a queue
<a name="discard-2"></a>

### discard/2 ###


<pre><code>
discard(Channel :: <a href="#type-channel">channel()</a>, Tag :: binary()) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


Discard message by delivery tag
<a name="get_msg-0"></a>

### get_msg/0 ###

`get_msg() -> any()`


<a name="get_msg-2"></a>

### get_msg/2 ###


<pre><code>
get_msg(Channel :: <a href="#type-channel">channel()</a>, Queue :: binary()) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


Get messges from queue
<a name="get_msg-3"></a>

### get_msg/3 ###


<pre><code>
get_msg(Channel :: <a href="#type-channel">channel()</a>,Queue :: binary(),NoAck :: boolean()) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


Get messges from queue, optionally setting noack option
<a name="new_channel-1"></a>

### new_channel/1 ###


<pre><code>
new_channel(Name :: <a href="#type-channel">channel()</a>) -&gt; {ok, pid()} | {error, term()}
</code></pre>


start a new channel on default rabbit connection
<a name="new_channel-2"></a>

### new_channel/2 ###


<pre><code>
new_channel(Rabbit :: <a href="#type-rabbit">rabbit()</a>, Name :: <a href="#type-channel">channel()</a>) -&gt;{ok, pid()} | {error, term()}
</code></pre>


start a new channel on a rabbit connection
<a name="publish-4"></a>

### publish/4 ###


<pre><code>
publish(Channel :: <a href="#type-channel">channel()</a>,Exchange :: <a href="#type-exchange">exchange()</a>,Key :: <a href="#type-routing_key">routing_key()</a>,Event :: <a href="#type-event">event()</a>) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


publish a message
<a name="reject-2"></a>

### reject/2 ###


<pre><code>
reject(Channel :: <a href="#type-channel">channel()</a>, Tag :: binary()) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


Reject message by delivery tag
<a name="start-0"></a>

### start/0 ###


<pre><code>
start() -&gt; ok
</code></pre>


start usagi application.
<a name="start_exchange-3"></a>

### start_exchange/3 ###


<pre><code>
start_exchange(Channel :: <a href="#type-channel">channel()</a>,Name :: <a href="#type-exchange">exchange()</a>,Type :: binary()) -&gt;<a href="#type-whynot">whynot()</a>
</code></pre>


start an exchange
<a name="start_queue-2"></a>

### start_queue/2 ###


<pre><code>
start_queue(Channel :: <a href="#type-channel">channel()</a>, Queue :: <a href="#type-r_queue">r_queue()</a>) -&gt; <a href="#type-whynot">whynot()</a>
</code></pre>


start a queue
<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; ok
</code></pre>


stop usagi application.
<a name="unpack-1"></a>

### unpack/1 ###

`unpack(Basic.consume_ok) -> any()`


