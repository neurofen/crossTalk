-module(message_store).


-export([start/0, stop/0]).

-export([delete_messages/1, get_messages/1, store_message/2, init_store/0]).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, message_store).

-record(chat_message,
	{addressee,
	 body,
	 created_on}).


start() ->
  server_util:start(?SERVER, {message_store, run, []}).

stop() ->
  server_util:stop(?SERVER).

run() ->
  ok.

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
  mnesia:transaction(F).

store_message(Addressee, Body) ->
  F = fun() ->
	{_, CreatedOn, _} = erlang:now(),
	mnesia:write(#chat_message{addressee=Addressee, body=Body, created_on=CreatedOn}) end,
  mnesia:transaction(F).

init_store() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(type, chat_message)
  catch
    exit: _ ->
      mnesia:create_table(chat_message, [ {attributes, record_info(fields, chat_message)},
					  {type, bag},
					  {disc_copies, [node()]}])
  end.