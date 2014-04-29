%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc RabbitMQ records
%%%
%%% @end
%%% Created : 29 Apr 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(usagi_recs).

-compile({parse_transform, exprecs}).

-export_records(['exchange.declare', 'exchange.delete', 'queue.declare', 'basic.qos', 'basic.get', 'basic.reject', 'basic.ack', 'basic.cancel', 'basic.consume', 'queue.bind', 'exchange.bind', 'basic.publish', 'queue.delete', 'exchange.delete_ok', 'exchange.declare_ok', 'queue.declare_ok', 'basic.qos_ok', 'basic.get_ok', 'basic.cancel_ok', 'basic.consume_ok', 'queue.bind_ok', 'exchange.bind_ok', 'queue.delete_ok', 'P_basic']).

-include_lib("amqp_client/include/amqp_client.hrl").
