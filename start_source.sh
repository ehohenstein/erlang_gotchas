#!/bin/bash

erl -sname source -setcookie monster -eval "gotchas:start_source('sink@sinkhost', [$SOURCE_FLAGS])"

