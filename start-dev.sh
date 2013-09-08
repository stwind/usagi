#!/bin/sh
ERL=erl
COOKIE=usagi
NODE_NAME=usagi@127.0.0.1
CONFIG=priv/app.config
LIBS_DIR="deps"

exec $ERL -pa ebin \
    -boot start_sasl \
    -name $NODE_NAME \
    -setcookie $COOKIE \
    -env ERL_LIBS $LIBS_DIR \
    -config $CONFIG \
    -s usagi
