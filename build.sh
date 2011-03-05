#!/bin/bash

erlc -o ebin src/*.erl

erl -pa ebin