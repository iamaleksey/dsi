-module(dsi_drv_tests).


-include_lib("eunit/include/eunit.hrl").
-include("dsi_msg.hrl").


-define(MOD_ID, 16#2d).
-define(MSG,
    #dsi_msg{
        hdr = #dsi_hdr{
            type     = 65533,
            id       = 65534,
            instance = 65535,
            src      = 253,
            dst      = 254,
            status   = 255,
            err_info = 4294967295
        },
        body = <<1, 2, 3, 4>>
    }).


dsi_drv_test_() ->
    {setup,
        fun() ->
                {ok, Pid} = dsi_drv:start_link(),
                Pid
        end,
        fun dsi_drv:stop/1,
        fun(Pid) ->
                {with, Pid, [
                        fun test_send/1,
                        fun test_recv/1,
                        fun test_grab_non_empty/1,
                        fun test_grab_empty/1,
                        fun test_is_congested/1,
                        fun test_link/1,
                        fun test_unlink/1
                ]}
        end
    }.


test_send(Pid) ->
    ?assertEqual(ok, dsi_drv:send(Pid, ?MOD_ID, ?MSG)),
    dsi_drv:recv(Pid, ?MOD_ID). % empty the queue.


test_recv(Pid) ->
    dsi_drv:send(Pid, ?MOD_ID, ?MSG),
    ?assertEqual({ok, ?MSG}, dsi_drv:recv(Pid, ?MOD_ID)).


test_grab_non_empty(Pid) ->
    dsi_drv:send(Pid, ?MOD_ID, ?MSG),
    ?assertEqual({ok, ?MSG}, dsi_drv:grab(Pid, ?MOD_ID)).


test_grab_empty(Pid) ->
    ?assertEqual(empty, dsi_drv:grab(Pid, ?MOD_ID)).


test_is_congested(Pid) ->
    ?assertEqual(false, dsi_drv:is_congested(Pid, standard)),
    ?assertEqual(false, dsi_drv:is_congested(Pid, long)).


test_link(Pid) ->
    ?assertEqual(true, dsi_drv:link(Pid)).


test_unlink(Pid) ->
    ?assertEqual(ok, dsi_drv:unlink(Pid)).
