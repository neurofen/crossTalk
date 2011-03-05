-module(boot).

-export([up/0]).

up() ->
  message_router:start(),
  mucc:start(),
  web_server:start(9999).