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
-module(redis_sd_epmd_service_config).

-include("redis_sd_epmd.hrl").

%% API
-export([wrap/1, merge_txtdata/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

wrap(S) ->
	Funs = [
		fun config_port/1,
		fun config_txtdata/1
	],
	lists:foldl(fun(F, Acc) ->
		F(Acc)
	end, S, Funs).

merge_txtdata(TXTData) ->
	PortString = case redis_sd_epmd:node_port() of
		Port when is_integer(Port) ->
			integer_to_list(Port);
		_ ->
			""
	end,
	[
		{"protocol", "epmd"},
		{"version", "0"},
		{"node", atom_to_list(node())},
		{"port", PortString}
		| val(TXTData)
	].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
config_port(S) ->
	case lists:keyfind(port, 1, S) of
		false ->
			[{port, fun redis_sd_epmd:erl_epmd_port/0} | S];
		_ ->
			S
	end.

%% @private
config_txtdata(S) ->
	case lists:keyfind(txtdata, 1, S) of
		false ->
			[{txtdata, {fun redis_sd_epmd_service_config:merge_txtdata/1, [[]]}} | S];
		{txtdata, TXTData} ->
			[{txtdata, {fun redis_sd_epmd_service_config:merge_txtdata/1, [TXTData]}} | S]
	end.

%% @private
val({Module, Function, Arguments}) when is_atom(Module) andalso is_atom(Function) andalso is_list(Arguments) ->
	erlang:apply(Module, Function, Arguments);
val({Function, Arguments}) when is_function(Function) andalso is_list(Arguments) ->
	erlang:apply(Function, Arguments);
val(Function) when is_function(Function, 0) ->
	Function();
val(Val) ->
	Val.
