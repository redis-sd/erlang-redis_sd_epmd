%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet

-module(smoke_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(REDIS_NS, "smoke_SUITE:").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([
	smoke/1
]).

all() ->
	[
		{group, tcp_simple},
		{group, unix_simple}
	].

groups() ->
	Tests = [
		smoke
	],
	[
		{tcp_simple, [parallel], Tests},
		{unix_simple, [parallel], Tests}
	].

init_per_suite(Config) ->
	ok = application:start(redis_test_server),
	ok = application:start(hierdis),
	ok = application:start(redis_sd_client),
	ok = application:start(redis_sd_server),
	ok = application:start(redis_sd_epmd),
	Config.

end_per_suite(_Config) ->
	application:stop(redis_sd_epmd),
	application:stop(redis_sd_server),
	application:stop(redis_sd_client),
	application:stop(hierdis),
	application:stop(redis_test_server),
	ok.

init_per_group(Name, Config) ->
	ct:log("starting ~s server...", [Name]),
	Options = redis_options_for_group(Name),
	{ok, _Pid} = redis_test_server:start_listener(Name, Options),
	ct:log("started"),
	[{redis_ref, Name} | Config].

end_per_group(Name, _Config) ->
	ct:log("stopping ~s server...", [Name]),
	redis_test_server:stop_listener(Name),
	ct:log("stopped"),
	ok.

%%====================================================================
%% Tests
%%====================================================================

smoke(Config) ->
	Name = ?config(redis_ref, Config),
	ok = redis_sd_epmd_event:add_handler(redis_sd_event_handler, self()),
	{Browse, BrowseConfig} = browse_for_group(Name),
	{Service, ServiceConfig} = service_for_group(Name),
	[] = redis_sd_epmd:nodes(),
	{ok, _} = redis_sd_epmd:new_browse(BrowseConfig),
	ok = ensure_whereis(Browse, 100, 10),
	[{Browse,[]}] = redis_sd_epmd:nodes(),
	{ok, _} = redis_sd_epmd:new_service(ServiceConfig),
	ok = ensure_whereis(Service, 100, 10),
	Node = node(),
	ok = wait_for_message({'$redis_sd', {epmd, nodeadd, Browse, Node}}, 5000),
	ok = wait_for_message({'$redis_sd', {epmd, nodeup, Browse, Node}}, 5000),
	case redis_sd_epmd:nodes() of
		[{Browse, [{Node, {up, _}}]}] ->
			ok;
		Nodes0 ->
			ct:fail(
				"Expected ~p to be found in nodes list.~n"
				"Actual: ~p",
				[Node, Nodes0])
	end,
	ok = redis_sd_epmd:rm_service(Service),
	ok = wait_for_message({'$redis_sd', {epmd, noderemove, Browse, Node}}, 5000),
	case redis_sd_epmd:nodes() of
		[{Browse, []}] ->
			ok;
		Nodes1 ->
			ct:fail(
				"Expected ~p to be found in nodes list.~n"
				"Actual: ~p",
				[Node, Nodes1])
	end,
	ok = redis_sd_epmd:rm_browse(Browse),
	ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
browse_for_group(tcp_simple) ->
	{tcp_simple_browse, [
		{name, tcp_simple_browse},
		{service, "tcp-simple"},
		{type, "tcp"},
		{domain, "local"},
		{redis_ns, ?REDIS_NS},
		{redis_opts, {tcp, ["127.0.0.1", redis_test_server:get_port(tcp_simple)]}}
	]};
browse_for_group(unix_simple) ->
	{unix_simple_browse, [
		{name, unix_simple_browse},
		{service, "unix-simple"},
		{type, "tcp"},
		{domain, "local"},
		{redis_ns, ?REDIS_NS},
		{redis_opts, {unix, [redis_test_server:get_path(unix_simple)]}}
	]}.

%% @private
service_for_group(tcp_simple) ->
	{tcp_simple_service, [
		{name, tcp_simple_service},
		{service, "tcp-simple"},
		{type, "tcp"},
		{domain, "local"},
		{redis_ns, ?REDIS_NS},
		{redis_opts, {tcp, ["127.0.0.1", redis_test_server:get_port(tcp_simple)]}}
	]};
service_for_group(unix_simple) ->
	{unix_simple_service, [
		{name, unix_simple_service},
		{service, "unix-simple"},
		{type, "tcp"},
		{domain, "local"},
		{redis_ns, ?REDIS_NS},
		{redis_opts, {unix, [redis_test_server:get_path(unix_simple)]}}
	]}.

%% @private
redis_options_for_group(tcp_simple) ->
	[{tcp, true}];
redis_options_for_group(unix_simple) ->
	[{unix, true}].

%% @private
ensure_whereis(Ref, Wait, {Retried, Retried}) ->
	case erlang:whereis(Ref) of
		Pid when is_pid(Pid) ->
			ok;
		Other ->
			ct:fail(
				"erlang:whereis/1 failed for ~p after waiting ~pms~n"
				"Returned: ~p",
				[Ref, Wait * Retried, Other])
	end;
ensure_whereis(Ref, Wait, {Retries, Retried}) ->
	case erlang:whereis(Ref) of
		Pid when is_pid(Pid) ->
			ok;
		_ ->
			timer:sleep(Wait),
			ensure_whereis(Ref, Wait, {Retries, Retried + 1})
	end;
ensure_whereis(Ref, Wait, Retries) ->
	ensure_whereis(Ref, Wait, {Retries, 0}).

%% @private
wait_for_message(Message, Timeout) ->
	receive
		Message ->
			ok
	after
		Timeout ->
			receive
				Received ->
					ct:fail(
						"Waited ~pms for message.~n"
						"Expected: ~p~n"
						"Received: ~p",
						[Timeout, Message, Received])
			after
				0 ->
					ct:fail(
						"Waited ~pms for message, but received nothing.~n"
						"Expected: ~p",
						[Timeout, Message])
			end
	end.
