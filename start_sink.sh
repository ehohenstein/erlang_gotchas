#!/bin/bash

erl -sname sink -setcookie monster -noshell -eval "gotchas:start_sink()"
