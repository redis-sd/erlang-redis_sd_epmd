%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  03 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-ifndef(REDIS_SD_EPMD_HRL).

-include_lib("redis_sd_client/include/redis_sd_client.hrl").
-include_lib("redis_sd_server/include/redis_sd_server.hrl").

-ifndef(EPMD_PORT_NO).
-define(EPMD_PORT_NO, 4369).
-endif.

-define(REDIS_SD_EPMD_HRL, 1).

-endif.
