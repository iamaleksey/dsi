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
