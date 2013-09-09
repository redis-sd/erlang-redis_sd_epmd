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

-include_lib("redis_sd_spec/include/redis_sd.hrl").
-include("redis_sd_epmd.hrl").

%% API
-export([wrap/1, make_instance/2, make_target/1, merge_txtdata/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

wrap(S) ->
	Funs = [
		{domain, "local"},
		{type, "tcp"},
		{service, "erlang"},
		{instance, {fun redis_sd_epmd_service_config:make_instance/2, [proplists:get_value(name, S)]}},
		{ttl, 120},
		{priority, 0},
		{weight, 0},
		{port, fun redis_sd_epmd:erl_epmd_port/0},
		{target, fun redis_sd_epmd_service_config:make_target/1},
		fun config_txtdata/1
	],
	lists:foldl(fun
		(F, Acc) when is_function(F) ->
			F(Acc);
		({Key, Val}, Acc) ->
			case lists:keyfind(Key, 1, Acc) of
				false ->
					[{Key, Val} | Acc];
				_ ->
					Acc
			end
	end, S, Funs).

make_instance(_DNSSD, Name) ->
	lists:flatten(io_lib:format("~s.~s", [node(), Name])).

make_target(#dns_sd{domain=Domain}) ->
	{ok, Hostname} = inet:gethostname(),
	redis_sd_ns:join([Hostname, Domain]).

merge_txtdata(DNSSD, TXTData) ->
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
		| val(DNSSD, TXTData)
	].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
config_txtdata(S) ->
	case lists:keyfind(txtdata, 1, S) of
		false ->
			[{txtdata, {fun redis_sd_epmd_service_config:merge_txtdata/2, [[]]}} | S];
		{txtdata, TXTData} ->
			[{txtdata, {fun redis_sd_epmd_service_config:merge_txtdata/2, [TXTData]}} | S]
	end.

%% @private
val(Obj, {Module, Function, Arguments}) when is_atom(Module) andalso is_atom(Function) andalso is_list(Arguments) ->
	erlang:apply(Module, Function, [Obj | Arguments]);
val(Obj, {Function, Arguments}) when is_function(Function) andalso is_list(Arguments) ->
	erlang:apply(Function, [Obj | Arguments]);
val(Obj, Function) when is_function(Function, 1) ->
	Function(Obj);
val(_Obj, Function) when is_function(Function, 0) ->
	Function();
val(_Obj, Val) ->
	Val.
