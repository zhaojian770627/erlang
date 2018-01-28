%% 服务器端mm 在启动服务时创建
%% 见start_port_instance这个函数会在start_port_serer中
%% 作为Fun传入，当accept一个新链接时（见start_accept）会
%% 新创建一个进程，在start_child中会调用Fun(即start_port_instance)
%% 这个进程就是MM

-module(chat_server).
-import(lib_chan_mm,[send/2,controller/2]).
-import(lists,[delete/2,foreach/2,map/2,member/2,reverse/2]).

-compile(export_all).

start()->
    start_server(),
    lib_chan:start_server("chat.conf").

start_server()->
    register(chat_server,
	     spawn(fun()->
			  process_flag(trap_exit,true),
			  Val=(catch server_loop([])),
			  io:format("Server terminated with:~p~n",[Val])
		  end)).

server_loop(L)->
    receive
	{mm,Channel,{login,Group,Nick}}->
	    case lookup(Group,L) of
		{ok,Pid}->
		    Pid!{login,Channel,Nick},
		    server_loop(L);
		error ->
		    Pid=spawn_link(fun()->
					   chat_group:start(Channel,Nick)
				   end),
		    server_loop([{Group,Pid}|L])
	    end;
	{mm_closed,_} ->
		server_loop(L);
	{'EXIT',Pid,allGone} ->
		L1=remove_group(Pid,L),
		server_loop(L1);
	Msg ->
		io:format("Server received Msg=~p~n",[Msg]),
		server_loop(L)
    end.

lookup(G,[{G,Pid}|_])->
    {ok,Pid};
lookup(G,[_|T]) ->
    lookup(G,T);
lookup(_,[]) ->
    error.

remove_group(Pid,[{G,Pid}|T])->
    io:format("~p removed~n",[G]),
    T;
remove_group(Pid,[H|T])->
    [H|remove_group(Pid,T)];
remove_group(_,[]) ->
    [].
