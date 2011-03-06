-module(boot).

-export([up/0, down/0]).

up() ->
  message_router:start(),
  mucc:start(),
  web_server:start(9999),
  io:format("crosstalk ready ...").

down() ->
	io:format("shutting down crosstalk ..."),
	web_server:stop(),
	mucc:stop(),
	message_router:stop().