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
-module(redis_sd_epmd).
-behaviour(gen_server).

-include("redis_sd_epmd.hrl").

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% API
-export([manual_start/0, start_link/0, add_host/2, del_host/1, nodes/0, nodes/1, set_browser/2, get_browser/1]).

%% Browse API
-export([new_browse/1, rm_browse/1, delete_browse/1]).

%% Service API
-export([new_service/1, rm_service/1, delete_service/1]).

%% Node API
-export([erl_epmd_port/0, node_host/0, node_host/1, node_join/2, node_name/0, node_name/1, node_port/0, node_port/1, node_split/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Manually start redis_sd_epmd and all dependencies.
-spec manual_start() -> ok.
manual_start() ->
	redis_sd:require([
		hierdis,
		redis_sd_client,
		redis_sd_server,
		redis_sd_epmd
	]).

%% @private
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

nodes() ->
	[{Ref, redis_sd_epmd:nodes(Ref)} || [Ref] <- ets:match(?TAB, {{browser, '$1'}, '_'})].

nodes(BrowseName) ->
	redis_sd_browser:call(BrowseName, nodelist).

add_host(Address, Host) ->
	gen_server:call(?SERVER, {add_host, Address, Host}).

del_host(Host) ->
	gen_server:call(?SERVER, {del_host, Host}).

set_browser(Ref, Pid) ->
	true = gen_server:call(?SERVER, {set_browser, Ref, Pid}),
	ok.

get_browser(Ref) ->
	ets:lookup_element(?TAB, {browser, Ref}, 2).

%%%===================================================================
%%% Browse API functions
%%%===================================================================

new_browse(BrowseConfig) ->
	redis_sd_client:new_browse(redis_sd_epmd_browse_config:wrap(BrowseConfig)).

rm_browse(BrowseName) ->
	redis_sd_client:rm_browse(BrowseName).

delete_browse(BrowseName) ->
	redis_sd_client:delete_browse(BrowseName).

%%%===================================================================
%%% Service API functions
%%%===================================================================

new_service(ServiceConfig) ->
	redis_sd_server:new_service(redis_sd_epmd_service_config:wrap(ServiceConfig)).

rm_service(ServiceName) ->
	redis_sd_server:rm_service(ServiceName).

delete_service(ServiceName) ->
	redis_sd_server:delete_service(ServiceName).

%%%===================================================================
%%% Node API functions
%%%===================================================================

erl_epmd_port() ->
	case os:getenv("ERL_EPMD_PORT") of
		false ->
			?EPMD_PORT_NO;
		StringPort ->
			case catch list_to_integer(StringPort) of
				Port when is_integer(Port) ->
					Port;
				_ ->
					?EPMD_PORT_NO
			end
	end.

node_host() ->
	node_host(node()).

node_host(Node) ->
	{_Name, Host} = node_split(Node),
	Host.

node_join(Name, Host) ->
	list_to_atom(Name ++ "@" ++ Host).

node_name() ->
	node_name(node()).

node_name(Node) ->
	{Name, _Host} = node_split(Node),
	Name.

node_port() ->
	node_port(node()).

node_port(Node) ->
	{Name, Host} = node_split(Node),
	port_please(Name, [Host, {getaddr, inet, Host}, {getaddr, inet6, Host}, localhost]).

node_split(Atom) when is_atom(Atom) ->
	node_split(atom_to_list(Atom));
node_split(Binary) when is_binary(Binary) ->
	node_split(binary_to_list(Binary));
node_split(List) when is_list(List) ->
	{Name, "@" ++ Host} = lists:splitwith(fun
		($@) ->
			false;
		(_) ->
			true
	end, List),
	{Name, Host}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} || [Ref, Pid] <- ets:match(?TAB, {{browser, '$1'}, '$2'})],
	{ok, #state{monitors=Monitors}}.

%% @private
handle_call({add_host, Address, Host}, _From, State) ->
	LHost = inet_db:tolower(Host),
	case ets:select_count(inet_hosts_byaddr, [{{LHost, '_', Address}, [], [true]}]) of
		0 ->
			LHosts = ets:select(inet_hosts_byaddr, [{{'$1', '_', Address}, [{'/=', '$1', LHost}], ['$1']}]),
			_ = inet_db:add_host(Address, [LHost | LHosts]),
			{reply, ok, State};
		_ ->
			{reply, ok, State}
	end;
handle_call({del_host, Host}, _From, State) ->
	LHost = inet_db:tolower(Host),
	Addresses = gb_sets:from_list(ets:select(inet_hosts_byaddr, [{{LHost, '_', '$1'}, [], ['$1']}])),
	ok = gb_sets:fold(fun(Address, _OK) ->
		case ets:select(inet_hosts_byaddr, [{{'$1', '_', Address}, [{'/=', '$1', LHost}], ['$1']}]) of
			[] ->
				inet_db:del_host(Address);
			LHosts ->
				inet_db:add_host(Address, LHosts)
		end
	end, ok, Addresses),
	{reply, ok, State};
handle_call({set_browser, Ref, Pid}, _From, State=#state{monitors=Monitors}) ->
	case ets:insert_new(?TAB, {{browser, Ref}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			{reply, true, State#state{monitors=[{{MonitorRef, Pid}, Ref} | Monitors]}};
		false ->
			{reply, false, State}
	end;
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', MonitorRef, process, Pid, _},
		State=#state{monitors=Monitors}) ->
	{_, Ref} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
	true = ets:delete(?TAB, {browser, Ref}),
	Monitors2 = lists:keydelete({MonitorRef, Pid}, 1, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info(Info, State) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, self(), handle_info, 2, Info]),
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
port_please(_Name, []) ->
	undefined;
port_please(Name, [localhost | Hosts]) ->
	case net_adm:localhost() of
		Host when is_list(Host) ->
			port_please(Name, [Host, {getaddr, inet, Host}, {getaddr, inet6, Host} | Hosts]);
		_ ->
			port_please(Name, Hosts)
	end;
port_please(Name, [{getaddr, Type, Host} | Hosts]) when is_atom(Type) ->
	case inet:getaddr(Host, Type) of
		{ok, Addr} ->
			case port_please(Name, [Addr]) of
				Port when is_integer(Port) ->
					Port;
				_ ->
					port_please(Name, Hosts)
			end;
		_ ->
			port_please(Name, Hosts)
	end;
port_please(Name, [Host | Hosts]) ->
	case erl_epmd:port_please(Name, Host) of
		{port, Port, _Version} ->
			Port;
		_ ->
			port_please(Name, Hosts)
	end.
