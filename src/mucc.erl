-module(mucc).

-define(SERVER, mucc).

-export([start/0, stop/0, server_loop/1, proxy_client/1]).
-export([register_nickname/1, send_message/3, poll/1]).


start() ->
  server_util:start(?SERVER, {mucc, server_loop, [dict:new()]}).


stop() ->
  server_util:stop(?SERVER).

%%----------------------------------------------------------------
%% Register nickname with message_router
%%----------------------------------------------------------------
register_nickname(Nickname) ->
  global:send(?SERVER, {register, Nickname, self()}),
  receive
    ok ->
		ok;
    {error, Error} ->
		Error
  end.

%%----------------------------------------------------------------
%% Unregister nickname with message_router
%%----------------------------------------------------------------
unregister_nickname(Nickname) ->
  global:send(?SERVER, {unregister, Nickname}).

%%----------------------------------------------------------------
%% Poll for messages from message_router from a Nickname
%%----------------------------------------------------------------
poll(Nickname) ->
  global:send(?SERVER, {poll, Nickname, self()}),
  receive
    {ok, Messages} ->
	  Messages;
    Error ->
	  Error
  end.

%%----------------------------------------------------------------
%% Send message to message_router
%%----------------------------------------------------------------
send_message(Sender, Addressee, Message) ->
  global:send(?SERVER, {send_message, Sender, Addressee, Message}).


server_loop(Proxies) ->
  receive
    {register, Nickname, Caller} ->
		case dict:find(Nickname, Proxies) of
		    error ->
			  Pid = spawn(fun() -> 
								process_flag(trap_exit, true),
								proxy_client([]) end),
			  erlang:monitor(process, Pid),
			  message_router:register_nick(Nickname, Pid),
			  Caller ! ok,
			  server_loop(dict:store(Nickname, Pid, Proxies));

		    {ok, _} ->
			  Caller ! {error, duplicate_nickname_found},
			  server_loop(Proxies)
		end;
		
	{unregister, Nickname} ->
		case dict:find(Nickname, Proxies) of
			error ->
				server_loop(Proxies);
			{ok, Pid} ->
				Pid ! stop,
				server_loop(dict:erase(Nickname, Proxies))
		end;

    {send_message, Sender, Addressee, Message} ->
		case dict:find(Sender, Proxies) of
		    error ->
			  ok;

		    {ok, Pid} ->
			  Pid ! {send_message, Addressee, Message}
		end,
		server_loop(Proxies);

    {poll, Nickname, Caller} ->
		case dict:find(Nickname, Proxies) of
		    error ->
			Caller ! {error, unknown_nickname};
		    {ok, Pid} ->
			Pid ! {get_messages, self()},
			receive
			    {messages, Messages} ->
					Caller ! {ok, Messages}
			end
		end,
		server_loop(Proxies)
  end.

%%----------------------------------------------------------------
%% Proxy client (web_server) to message_router
%%----------------------------------------------------------------

proxy_client(Messages) ->
  receive
    {printmsg, MessageBody} ->
	proxy_client([MessageBody|Messages]);
    {get_messages, Caller} ->
		Caller ! {messages, lists:reverse(Messages)},
		proxy_client([]);
    {send_message, Addressee, Message} ->
		message_router:send_chat_msg(Addressee, Message),
		proxy_client(Messages);
    stop ->
		io:format("Proxy stopping ...~n"),
		ok
  end.

