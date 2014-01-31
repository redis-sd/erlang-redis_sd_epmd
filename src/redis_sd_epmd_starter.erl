%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  04 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_epmd_starter).

%% API
-export([start_link/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	%% a list of browse configs
	Browses = case application:get_env(redis_sd_epmd, browses) of
		{ok, B} ->
			B;
		undefined ->
			[]
	end,
	%% a list of service configs
	Services = case application:get_env(redis_sd_epmd, services) of
		{ok, S} ->
			S;
		undefined ->
			[]
	end,
	_ = [redis_sd_epmd:new_browse(Browse) || Browse <- Browses],
	_ = [redis_sd_epmd:new_service(Service) || Service <- Services],
	ignore.
