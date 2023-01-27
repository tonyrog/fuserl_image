%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    SFTP client wrapper to read/write data to/from backup server
%%% @end
%%% Created : 25 Jan 2023 by Tony Rogvall <tony@rogvall.se>

-module(fuserl_image_sftp).

-export([connect/2, connect/3]).
-export([disconnect/1]).
-export([open/3, close/1]).
-export([pwrite/3, pread/3]).

%% example options:
%%   [{user, "joe"}, {password, "pass"}]
%%   [{user, "klacke"}, {rsa_pass_phrase, "foobar"}]
%%   [{silently_accept_hosts, true}] ...
%%    
%% 
connect(Host, Options) ->
    connect(Host, 22, Options).
connect(Host, Port, Options) ->
    case ssh:connect(Host, Port, Options) of
	{ok,ConnectionRef} ->
	    case ssh_sftp:start_channel(ConnectionRef) of
		{ok,ChannelPid} ->
		    {ok,{ChannelPid,ConnectionRef}};
		Error ->
		    ssh:close(ConnectionRef),
		    Error
	    end;
	Error ->
	    Error
    end.

disconnect({ChannelPid,ConnectionRef}) ->
    ssh_sftp:stop_channel(ChannelPid),
    ssh:close(ConnectionRef);
%% allow close channel with file handle as well?
disconnect({_Handle,ChannelPid,ConnectionRef}) ->
    ssh_sftp:stop_channel(ChannelPid),
    ssh:close(ConnectionRef).

%% Open a remote file
open({ChannelPid,ConnectionRef}, Name, Mode) ->
    case ssh_sftp:open(ChannelPid, Name, Mode) of
	{ok, Handle} ->
	    {ok, {Handle,ChannelPid,ConnectionRef}};
	Error ->
	    Error
    end.

%% Close a remote file
close(_Fd={Handle,ChannelPid,_ConnectionRef}) ->
    ssh_sftp:close(ChannelPid,Handle).

%% Write data
pwrite(_Fd={Handle,ChannelPid,_ConnectionRef}, Position, Data) ->
    ssh_sftp:pwrite(ChannelPid, Handle, Position, Data).

pread(_Fd={Handle,ChannelPid,_ConnectionRef}, Position, Len) ->
    ssh_sftp:pread(ChannelPid, Handle, Position, Len).
