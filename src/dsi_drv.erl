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


-type dsi_msg() :: #'dsi_msg'{}.
-type mod_id()  :: non_neg_integer().


-define(APP_NAME, dsi).
-define(DRV_NAME, dsi_drv).


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


handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.


handle_cast(stop, St) ->
    {stop, normal, St};


handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.


handle_info(Request, St) ->
    {stop, {unexpected_info, Request}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
