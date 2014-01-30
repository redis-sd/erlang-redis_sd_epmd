%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  30 Jan 2014 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_epmd_config).

-include("redis_sd_epmd.hrl").

%% API
-export([list_to_browse/1, list_to_browse/2, list_to_service/1, list_to_service/2]).

%% Service API
-export([service_instance/2, service_port/1, service_target/1, service_txtdata/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec list_to_browse([{atom(), term()}]) -> redis_sd_browse().
list_to_browse(BrowseConfig) ->
	list_to_browse([], BrowseConfig).

-spec list_to_browse([module()], [{atom(), term()}]) -> redis_sd_browse().
list_to_browse(Apps, BrowseConfig) when is_list(Apps) ->
	Default = ?REDIS_SD_BROWSE{},
	Defaults = [
		{domain, Default?REDIS_SD_BROWSE.domain, app},
		{type, Default?REDIS_SD_BROWSE.type, app},
		{service, Default?REDIS_SD_BROWSE.service, app}
	],
	B = redis_sd_config:merge(Apps ++ [redis_sd_epmd], Defaults, BrowseConfig),
	redis_sd_client_config:list_to_browse(Apps ++ [redis_sd_epmd], B).

-spec list_to_service([{atom(), term()}]) -> redis_sd_service().
list_to_service(ServiceConfig) ->
	list_to_service([], ServiceConfig).

-spec list_to_service([module()], [{atom(), term()}]) -> redis_sd_service().
list_to_service(Apps, ServiceConfig) when is_list(Apps) ->
	_Default = ?REDIS_SD_SERVICE{},
	Defaults = [
		{domain, "local", app},
		{type, "tcp", app},
		{service, "erlang", app},
		{instance, {fun ?MODULE:service_instance/2, [proplists:get_value(name, ServiceConfig)]}, app},
		{ttl, 120, app},
		{priority, 0, app},
		{weight, 0, app},
		{port, fun ?MODULE:service_port/1, app},
		{target, fun ?MODULE:service_target/1, app},
		fun config_service_txtdata/1
	],
	S = redis_sd_config:merge(Apps ++ [redis_sd_epmd], Defaults, ServiceConfig),
	redis_sd_server_config:list_to_service(Apps ++ [redis_sd_epmd], S).

%%%===================================================================
%%% Service API functions
%%%===================================================================

service_instance(_Record, ServiceName) ->
	atom_to_list(node()) ++ "." ++ atom_to_list(ServiceName).

service_port(_Record) ->
	redis_sd_epmd:erl_epmd_port().

service_target(?REDIS_SD_DNS{domain=Domain}) ->
	{ok, Hostname} = inet:gethostname(),
	redis_sd_ns:join([Hostname, Domain]).

service_txtdata(Record, TXTData) ->
	NodePortString = case redis_sd_epmd:node_port() of
		NodePort when is_integer(NodePort) ->
			integer_to_list(NodePort);
		_ ->
			""
	end,
	[
		{"protocol", "epmd"},
		{"version", "1"},
		{"node", atom_to_list(node())},
		{"port", NodePortString}
		| redis_sd_server_dns:val(Record, TXTData)
	].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
config_service_txtdata(S) ->
	case lists:keyfind(txtdata, 1, S) of
		false ->
			[{txtdata, {fun ?MODULE:service_txtdata/2, [[]]}} | S];
		{txtdata, TXTData} ->
			[{txtdata, {fun ?MODULE:service_txtdata/2, [TXTData]}} | S]
	end.
