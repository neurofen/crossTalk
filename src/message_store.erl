-module(message_store).


-export([start/0, stop/0]).

-export([delete_messages/1, get_messages/1, store_message/2, init_store/0]).

-export([save_message/2, find_messages/1, run/1]).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, message_store).

-record(chat_message,
	{addressee,
	 body,
	 created_on}).


save_message(Addressee, Body) ->
  global:send(?SERVER, {save_msg, Addressee, Body}).

find_messages(Addressee) ->
  global:send(?SERVER, {find_msgs, Addressee, self()}),
  receive
    {ok, Messages} ->
	  Messages
  end.

start() ->
  server_util:start(?SERVER, {message_store, run, [true]}).

stop() ->
  server_util:stop(?SERVER).

run(FirstTime) ->  
  if
    FirstTime == true ->
	init_store(),
	run(false);
    true ->
	receive
	  {save_msg, Addressee, Body} ->
	      store_message(Addressee, Body),
	      run(false);
	  {find_msgs, Addressee, Pid} ->
	      Messages = get_messages(Addressee),
	      Pid ! {ok, Messages},
	      run(false);
	  shutdown ->
	      mnesia:stop(),
	      io:format("Shutting down ... ~n")
	end
  end.

delete_messages(Messages) ->
  F = fun() ->
	lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages) end,
  mnesia:transaction(F).

get_messages(Addressee) ->
  F = fun() ->
	Query = qlc:q([M#chat_message.body || M <- mnesia:table(chat_message), M#chat_message.addressee =:= Addressee]),
	Results = qlc:e(Query),
	delete_messages(Results),
	Results end,
  {atomic, Messages} = mnesia:transaction(F),
  Messages.

store_message(Addressee, Body) ->
  F = fun() ->
	{_, CreatedOn, _} = erlang:now(),
	mnesia:write(#chat_message{addressee=Addressee, body=Body, created_on=CreatedOn}) end,
  mnesia:transaction(F).

init_store() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(chat_message, type)
  catch
    exit: _ ->
      mnesia:create_table(chat_message, [ {attributes, record_info(fields, chat_message)},
					  {type, bag},
					  {disc_copies, [node()]}])
  end.