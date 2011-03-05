-module(mucc).

-define(SERVER, mucc).

-export([start/0, stop/0, server_loop/1, proxy_client/1, poll/1]).
-export([register_nickname/1]).


start() ->
  server_util:start(?SERVER, {mucc, server_loop, [dict:new()]}).


stop() ->
  server_util:stop(?SERVER).

register_nickname(Nickname) ->
  global:send(?SERVER, {register, Nickname, self()}),
  receive
    ok ->
	ok;
    {error, Error} ->
	Error
  end.

poll(Nickname) ->
  global:send(?SERVER, {poll, Nickname, self()}),
  receive
    {ok, Messages} ->
	  Messages;
    Error ->
	  Error
  end.

server_loop(Proxies) ->
  receive
    {register, Nickname, Caller} ->
	case dict:find(Nickname, Proxies) of
	  error ->
		Pid = spawn(fun() -> proxy_client([]) end),
		message_router:register_nick(Nickname, Pid),
		Caller ! ok,
		server_loop(dict:store(Nickname, Pid, Proxies));

	  {ok, _} ->
		Caller ! {error, duplicate_nickname_found},
		server_loop(Proxies)
	end;

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

proxy_client(Messages) ->
  receive
    {printmsg, MessageBody} ->
	proxy_client([MessageBody|Messages]);
    {get_messages, Caller} ->
	Caller ! {messages, lists:reverse(Messages)},
	proxy_client([]);
    stop ->
	io:format("Proxy stopping ...~n"),
	ok
  end.

