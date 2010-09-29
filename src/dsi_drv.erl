%%% Copyright (c) 2010 Aleksey Yeschenko <aleksey@yeschenko.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.


-module(dsi_drv).


-behaviour(gen_server).


-include("dsi_msg.hrl").


%% API exports
-export([start_link/0,
         stop/1,
         send/3,
         recv/2,
         grab/2,
         link/1,
         unlink/1,
         is_congested/2]).


%% gen_server exports
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).


-type dsi_msg() :: #dsi_msg{}.
-type mod_id()  :: non_neg_integer().


-define(APP_NAME, dsi).
-define(DRV_NAME, "dsi_drv").

-define(DSI_SEND,         0).
-define(DSI_RECV,         1).
-define(DSI_GRAB,         2).
-define(DSI_LINK,         3).
-define(DSI_UNLINK,       4).
-define(DSI_IS_CONGESTED, 5).

-define(DSI_STANDARD, 0).
-define(DSI_LONG,     1).


-record(st, {port :: port()}).


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------


-spec start_link/0 :: () -> {'ok', pid()} | {'error', any()}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec stop/1 :: (pid()) -> 'ok'.

stop(Pid) ->
    gen_server:cast(Pid, stop).


-spec send/3 :: (pid(), mod_id(), dsi_msg()) -> 'ok' | 'error'.

send(Pid, ModId, Msg) ->
    gen_server:call(Pid, {send, ModId, Msg}, infinity).


-spec recv/2 :: (pid(), mod_id()) -> {'ok', dsi_msg()} | 'error'.

recv(Pid, ModId) ->
    gen_server:call(Pid, {recv, ModId}, infinity).


-spec grab/2 :: (pid(), mod_id()) -> {'ok', dsi_msg()} | 'empty'.

grab(Pid, ModId) ->
    gen_server:call(Pid, {grab, ModId}, infinity).


-spec link/1 :: (pid()) -> boolean().

link(Pid) ->
    gen_server:call(Pid, link, infinity).


-spec unlink/1 :: (pid()) -> 'ok'.

unlink(Pid) ->
    gen_server:call(Pid, unlink, infinity).


-spec is_congested/2 :: (pid(), 'standard' | 'long') -> boolean().

is_congested(Pid, Type) ->
    gen_server:call(Pid, {is_congested, Type}, infinity).


%% -------------------------------------------------------------------------
%% gen_server callback functions
%% -------------------------------------------------------------------------


init([]) ->
    case erl_ddll:load(code:priv_dir(?APP_NAME), ?DRV_NAME) of
        ok ->
            Port = open_port({spawn_driver, ?DRV_NAME}, [binary]),
            {ok, #st{port = Port}};
        {error, Reason} ->
            {stop, Reason}
    end.


terminate(_Reason, St) ->
    port_close(St#st.port).


handle_call({send, ModId, Msg}, _From, St) ->
    #dsi_msg{
        hdr = #dsi_hdr{
            type     = Type,
            id       = Id,
            instance = Instance,
            src      = Src,
            dst      = Dst,
            status   = Status,
            err_info = ErrInfo
        },
        body = Body
    } = Msg,
    Hdr = <<Type:16, Id:16, Instance:16, Src:8, Dst:8, Status:8, ErrInfo:32>>,
    port_command(St#st.port, [?DSI_SEND, ModId, Hdr, Body]),
    {reply, port_reply(), St};


handle_call({recv, ModId}, _From, St) ->
    port_command(St#st.port, [?DSI_RECV, ModId]),
    {reply, port_reply(), St};


handle_call({grab, ModId}, _From, St) ->
    port_command(St#st.port, [?DSI_GRAB, ModId]),
    {reply, port_reply(), St};


handle_call(link, _From, St) ->
    port_command(St#st.port, [?DSI_LINK]),
    {reply, port_reply(), St};


handle_call(unlink, _From, St) ->
    port_command(St#st.port, [?DSI_UNLINK]),
    {reply, port_reply(), St};


handle_call({is_congested, Type}, _From, St) ->
    Byte =
        case Type of
            standard -> ?DSI_STANDARD;
            long     -> ?DSI_LONG
        end,
    port_command(St#st.port, [?DSI_IS_CONGESTED, Byte]),
    {reply, port_reply(), St};


handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.


handle_cast(stop, St) ->
    {stop, normal, St};


handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.


handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------


port_reply() ->
    receive
        {dsi_reply, Reply} -> Reply
    end.
