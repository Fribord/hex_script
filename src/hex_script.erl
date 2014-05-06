%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Hex script plugin 
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_script).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal(), Cb::function()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(_Flags, _Signal, _Cb) ->
    {error, no_input}.

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(_Ref) ->
    {error, no_input}.

%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, _Env) ->
    case proplists:get_value(command, Flags, undefined) of
	undefined -> ok;
	Command -> command(Command)
    end.

%%
%% init_event(in | out, Flags::[{atom(),term()}])
%%
init_event(_, _) ->
    ok.

%%
%% mod_event(in | out, Flags::[{atom(),term()}])
%%
mod_event(_, _) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(_Dir, _Flags) ->
    ok.

%%
%% return event specification in internal YANG format
%% {Type,Value,Stmts}
%%
event_spec(_Dir) ->
    [].


command(Command) when is_list(Command) ->
    Port = erlang:open_port({spawn,Command},[exit_status]),
    wait_exit(Port).

%% wait for command to exit properly
wait_exit(Port) ->
    wait_exit(Port,-1).

wait_exit(Port,Status) ->
    receive
	{Port, {exit_status,Status1}} ->
	    wait_exit(Port,Status1);
	{Port, eof} ->
	    erlang:port_close(Port),
	    Status;
	{Port,{data,_Data}} ->
	    wait_exit(Port,Status)
    end.


	
