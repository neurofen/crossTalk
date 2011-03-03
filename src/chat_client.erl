-module(chat_client).

-export([start_router/0, send_message/2, register_nickname/1, unregister_nickname/1, handle_messages/1]).

start_router() ->
  message_router:start().

register_nickname(Nickname) ->
  Pid = spawn(chat_client, handle_messages, [Nickname]),
  message_router:register_nick(Nickname, Pid).

unregister_nickname(Nickname) ->
  message_router:unregister_nick(Nickname).

send_message(Addressee, MessageBody) ->
  message_router:send_chat_msg(Addressee, MessageBody).

handle_messages(Nickname) ->
  receive
    {printmsg, MessageBody} ->
      io:format("~p Received: ~p~n", [Nickname, MessageBody]),
      handle_messages(Nickname);
    stop ->
      ok
  end.
  