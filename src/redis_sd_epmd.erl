%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
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
-export([manual_start/0, start_link/0]).
-export([add_host/2, del_host/1]).
-export([connected_node/4, nodes/0, nodes/1, enable/0, disable/0]).

%% Browse API
-export([new_browse/1, rm_browse/1, delete_browse/1, list_browses/0]).

%% Service API
-export([new_service/1, rm_service/1, delete_service/1, list_services/0]).

%% Node API
-export([erl_epmd_port/0, node_host/0, node_host/1, node_join/2,
	node_name/0, node_name/1, node_port/0, node_port/1, node_split/1]).

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
	ok = redis_sd_client:manual_start(),
	ok = redis_sd_server:manual_start(),
	redis_sd:require([
		redis_sd_epmd
	]).

%% @private
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_host(Address, Host) ->
	gen_server:call(?SERVER, {add_host, Address, Host}).

del_host(Host) ->
	gen_server:call(?SERVER, {del_host, Host}).

connected_node(Node, Key, BrowseRef, Status) when is_atom(Node) ->
	gen_server:cast(?SERVER, {connected_node, Node, Key, BrowseRef, Status}).

nodes() ->
	[{Node, Status} || [Node, Status] <- ets:match(?TAB, {{node, '$1'}, '_', '$2'})].

nodes(BrowseRef) ->
	[{Node, Status} || [Node, Status] <- ets:match(?TAB, {{node, '$1'}, {BrowseRef, '_'}, '$2'})].

enable() ->
	ok = redis_sd_client:enable([BrowseRef || {BrowseRef, _} <- list_browses()]),
	ok = redis_sd_server:enable([ServiceRef || {ServiceRef, _} <- list_services()]),
	ok.

disable() ->
	ok = redis_sd_client:disable([BrowseRef || {BrowseRef, _} <- list_browses()]),
	ok = redis_sd_server:disable([ServiceRef || {ServiceRef, _} <- list_services()]),
	ok.

%%%===================================================================
%%% Browse API functions
%%%===================================================================

new_browse(BrowseConfig) ->
	gen_server:call(?SERVER, {new_browse, redis_sd_epmd_config:list_to_browse(BrowseConfig)}).

rm_browse(BrowseName) ->
	redis_sd_client:rm_browse(BrowseName).

delete_browse(BrowseName) ->
	redis_sd_client:delete_browse(BrowseName).

list_browses() ->
	BrowseRefs = gb_sets:from_list([BrowseRef || [BrowseRef] <- ets:match(?TAB, {{pid, browse, '$1'}, '_'})]),
	[Browse || Browse={BrowseRef, _} <- redis_sd_client:list_browses(), gb_sets:is_element(BrowseRef, BrowseRefs)].

%%%===================================================================
%%% Service API functions
%%%===================================================================

new_service(ServiceConfig) ->
	gen_server:call(?SERVER, {new_service, redis_sd_epmd_config:list_to_service(ServiceConfig)}).

rm_service(ServiceName) ->
	redis_sd_server:rm_service(ServiceName).

delete_service(ServiceName) ->
	redis_sd_server:delete_service(ServiceName).

list_services() ->
	ServiceRefs = gb_sets:from_list([ServiceRef || [ServiceRef] <- ets:match(?TAB, {{pid, service, '$1'}, '_'})]),
	[Service || Service={ServiceRef, _} <- redis_sd_server:list_services(), gb_sets:is_element(ServiceRef, ServiceRefs)].

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
	Lookup = inet_db:res_option(lookup) -- [file],
	ok = inet_db:set_lookup([file | Lookup]),
	ok = net_kernel:monitor_nodes(true),
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} || [Ref, Pid] <- ets:match(?TAB, {{pid, '_', '$1'}, '$2'})],
	ok = redis_sd_client_event:add_handler(redis_sd_event_handler, self()),
	ok = redis_sd_server_event:add_handler(redis_sd_event_handler, self()),
	{ok, #state{monitors=Monitors}}.

%% @private
handle_call({new_browse, Browse=?REDIS_SD_BROWSE{ref=Ref}}, _From, State=#state{monitors=Monitors}) ->
	Reply = redis_sd_client:new_browse(Browse),
	Pid = case Reply of
		{ok, P} ->
			P;
		{error, {already_started, P}} ->
			P
	end,
	State2 = case ets:insert_new(?TAB, {{pid, browse, Ref}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			State#state{monitors=[{{MonitorRef, Pid}, {browse, Ref}} | Monitors]};
		false ->
			State
	end,
	{reply, Reply, State2};
handle_call({new_service, Service=?REDIS_SD_SERVICE{name=Ref}}, _From, State=#state{monitors=Monitors}) ->
	Reply = redis_sd_server:new_service(Service),
	Pid = case Reply of
		{ok, P} ->
			P;
		{error, {already_started, P}} ->
			P
	end,
	State2 = case ets:insert_new(?TAB, {{pid, service, Ref}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			State#state{monitors=[{{MonitorRef, Pid}, {service, Ref}} | Monitors]};
		false ->
			State
	end,
	{reply, Reply, State2};
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
	ok = inet_db_del_host(Host),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast({connected_node, Node, Key, BrowseRef, Status}, State) ->
	case ets:member(?TAB, {pid, browse, BrowseRef}) of
		false ->
			{noreply, State};
		true ->
			case ets:lookup(?TAB, {node, Node}) of
				[{{node, Node}, {BrowseRef, Key}, Status}] ->
					{noreply, State};
				[{{node, Node}, {BrowseRef, Key}, OldStatus}] ->
					case {OldStatus, Status} of
						{_, true} ->
							redis_sd_epmd_event:up(BrowseRef, Key, Node);
						{true, _} ->
							redis_sd_epmd_event:down(BrowseRef, Key, Node);
						_ ->
							ok
					end,
					true = ets:insert(?TAB, {{node, Node}, {BrowseRef, Key}, Status}),
					{noreply, State};
				_ ->
					case Status of
						true ->
							redis_sd_epmd_event:up(BrowseRef, Key, Node);
						_ ->
							redis_sd_epmd_event:down(BrowseRef, Key, Node)
					end,
					true = ets:insert(?TAB, {{node, Node}, {BrowseRef, Key}, Status}),
					{noreply, State}
			end
	end;
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'$redis_sd', {record, add, Key, Record, Browse}}, State) ->
	ok = maybe_connect(Key, Record, Browse),
	{noreply, State};
handle_info({'$redis_sd', {record, expire, Key, Record, Browse}}, State) ->
	ok = maybe_disconnect(Key, Record, Browse),
	{noreply, State};
handle_info({'$redis_sd', _Event}, State) ->
	{noreply, State};
handle_info({nodeup, Node}, State) ->
	case catch ets:lookup(?TAB, {node, Node}) of
		[{{node, Node}, {BrowseRef, Key}, Status}] when Status =/= true ->
			true = ets:insert(?TAB, {{node, Node}, {BrowseRef, Key}, true}),
			redis_sd_epmd_event:up(BrowseRef, Key, Node),
			{noreply, State};
		_ ->
			{noreply, State}
	end;
handle_info({nodedown, Node}, State) ->
	case catch ets:lookup(?TAB, {node, Node}) of
		[{{node, Node}, {BrowseRef, Key}, Status}] when Status =/= false ->
			true = ets:insert(?TAB, {{node, Node}, {BrowseRef, Key}, false}),
			redis_sd_epmd_event:down(BrowseRef, Key, Node),
			{noreply, State};
		_ ->
			{noreply, State}
	end;
handle_info({'DOWN', MonitorRef, process, Pid, _}, State=#state{monitors=Monitors}) ->
	case lists:keytake({MonitorRef, Pid}, 1, Monitors) of
		{value, {{MonitorRef, Pid}, {browse, Ref}}, Monitors2} ->
			true = ets:delete(?TAB, {pid, browse, Ref}),
			true = ets:match_delete(?TAB, {{node, '_'}, {Ref, '_'}, '_'}),
			{noreply, State#state{monitors=Monitors2}};
		{value, {{MonitorRef, Pid}, {service, Ref}}, Monitors2} ->
			true = ets:delete(?TAB, {pid, service, Ref}),
			{noreply, State#state{monitors=Monitors2}};
		false ->
			{noreply, State}
	end;
handle_info(Info, State) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, self(), handle_info, 2, Info]),
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	_ = net_kernel:monitor_nodes(false),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
get_valid_node(?REDIS_SD_DNS{txtdata=TXTData}) ->
	case get_values([<<"protocol">>, <<"version">>, <<"node">>], TXTData) of
		[<<"epmd">>, <<"1">>, Node] when is_binary(Node) ->
			binary_to_atom(Node, unicode);
		_ ->
			undefined
	end.

%% @private
get_values(Keys, Opts) ->
	gv(Keys, Opts, []).

%% @private
gv([], _Opts, Acc) ->
	lists:reverse(Acc);
gv([Key | Keys], Opts, Acc) ->
	case get_value(Key, Opts) of
		undefined ->
			undefined;
		Val ->
			gv(Keys, Opts, [Val | Acc])
	end.

%% @private
get_value(Key, Opts) ->
	get_value(Key, Opts, undefined).

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
	case lists:keyfind(Key, 1, Opts) of
		{_, Value} -> Value;
		_ -> Default
	end.

%% @private
inet_db_del_host(Host) ->
	LHost = inet_db:tolower(Host),
	Addresses = gb_sets:from_list(ets:select(inet_hosts_byaddr, [{{LHost, '_', '$1'}, [], ['$1']}])),
	gb_sets:fold(fun(Address, _OK) ->
		case ets:select(inet_hosts_byaddr, [{{'$1', '_', Address}, [{'/=', '$1', LHost}], ['$1']}]) of
			[] ->
				inet_db:del_host(Address);
			LHosts ->
				inet_db:add_host(Address, LHosts)
		end
	end, ok, Addresses).

%% @private
is_valid_node(Record, ?REDIS_SD_BROWSE{ref=BrowseRef}) ->
	case ets:member(?TAB, {pid, browse, BrowseRef}) of
		false ->
			ok;
		true ->
			case get_valid_node(Record) of
				Node when is_atom(Node) ->
					{true, Node};
				_ ->
					false
			end
	end.

%% @private
maybe_connect(Key, Record=?REDIS_SD_DNS{}, Browse=?REDIS_SD_BROWSE{ref=BrowseRef}) ->
	case is_valid_node(Record, Browse) of
		{true, Node} ->
			case ets:match(?TAB, {{node, Node}, {BrowseRef, Key}, '$1'}) of
				[[true]] ->
					ok;
				R ->
					case R of
						[] ->
							redis_sd_epmd_event:add(BrowseRef, Key, Node);
						_ ->
							ok
					end,
					_ = redis_sd_epmd_sup:start_worker(Node, Key, Record, Browse),
					ok
			end;
		false ->
			ok
	end.

%% @private
maybe_disconnect(Key, Record=?REDIS_SD_DNS{}, Browse=?REDIS_SD_BROWSE{ref=BrowseRef}) ->
	case is_valid_node(Record, Browse) of
		{true, Node} ->
			redis_sd_epmd_event:expire(BrowseRef, Key, Node),
			true = ets:delete(?TAB, {node, Node}),
			case catch redis_sd_epmd:node_split(Node) of
				{Name, Host} when is_list(Name) andalso is_list(Host) ->
					ok = inet_db_del_host(Host),
					_ = erlang:disconnect_node(Node),
					ok;
				_ ->
					ok
			end;
		false ->
			ok
	end.

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

%%%-------------------------------------------------------------------
%%% Test functions
%%%-------------------------------------------------------------------

-ifdef(TEST).

fake_node_test() ->
	Node = 'fake-node@fake-host',
	NodeBin = <<"fake-node@fake-host">>,
	{Name="fake-node", Host="fake-host"} = node_split(Node),
	{Name, Host} = node_split(NodeBin),
	Name = node_name(Node),
	Host = node_host(Node),
	Node = node_join(Name, Host),
	ok.

-endif.
