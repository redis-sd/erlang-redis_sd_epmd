Redis Service Discovery using Erlang epmd
=========================================

[![Build Status](https://travis-ci.org/redis-sd/erlang-redis_sd_epmd.png?branch=master)](https://travis-ci.org/redis-sd/erlang-redis_sd_epmd)

Configuration may be specified manually to `redis_sd_epmd:new_service/1` and `redis_sd_epmd:new_browse/1` or through `{browses, BrowseConfigs}` and `{services, ServiceConfigs}` env variables for `redis_sd_epmd`.

Here is a brief example of starting up 3 erlang nodes on the same host with different node and host names:

*`node1@host1`*

```erl
$ erl -pa deps/*/ebin ebin -sname node1@host1 -run redis_sd_epmd manual_start
Erlang R16B01 (erts-5.10.2) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.2  (abort with ^G)
1> ServiceConfig = [
1>     {name, test_service},
1>     {service, "test-service"}
1> ],
1> redis_sd_epmd:new_service(ServiceConfig).
{ok,<0.63.0>}
2> redis_sd_epmd:nodes().
[]
3> BrowseConfig = [
3>     {name, test_browse},
3>     {service, "test-service"}
3> ],
3> redis_sd_epmd:new_browse(BrowseConfig).
{ok,<0.70.0>}
4> redis_sd_epmd:nodes().
[{test_browse,[{node1@host1,{up,[{<<"protocol">>,<<"epmd">>},
                                 {<<"version">>,<<"0">>},
                                 {<<"node">>,<<"node1@host1">>},
                                 {<<"port">>,<<"64249">>}]}}]}]
```

*`node2@host2`*

```erl
$ erl -pa deps/*/ebin ebin -sname node2@host2 -run redis_sd_epmd manual_start
Erlang R16B01 (erts-5.10.2) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.2  (abort with ^G)
1> ServiceConfig = [
1>     {name, test_service},
1>     {service, "test-service"}
1> ],
1> redis_sd_epmd:new_service(ServiceConfig).
{ok,<0.63.0>}
2> redis_sd_epmd:nodes().
[]
3> BrowseConfig = [
3>     {name, test_browse},
3>     {service, "test-service"}
3> ],
3> redis_sd_epmd:new_browse(BrowseConfig).
{ok,<0.73.0>}
4> redis_sd_epmd:nodes().
[{test_browse,[{node1@host1,{up,[{<<"protocol">>,<<"epmd">>},
                                 {<<"version">>,<<"0">>},
                                 {<<"node">>,<<"node1@host1">>},
                                 {<<"port">>,<<"64249">>}]}},
               {node2@host2,{up,[{<<"protocol">>,<<"epmd">>},
                                 {<<"version">>,<<"0">>},
                                 {<<"node">>,<<"node2@host2">>},
                                 {<<"port">>,<<"64255">>}]}}]}]
```

*`node3@host3`*

```erl
$ erl -pa deps/*/ebin ebin -sname node3@host3 -run redis_sd_epmd manual_start
Erlang R16B01 (erts-5.10.2) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.2  (abort with ^G)
1> ServiceConfig = [
1>     {name, test_service},
1>     {service, "test-service"}
1> ],
1> redis_sd_epmd:new_service(ServiceConfig).
{ok,<0.63.0>}
2> redis_sd_epmd:nodes().
[]
3> BrowseConfig = [
3>     {name, test_browse},
3>     {service, "test-service"}
3> ],
3> redis_sd_epmd:new_browse(BrowseConfig).
{ok,<0.77.0>}
4> redis_sd_epmd:nodes().
[{test_browse,[{node1@host1,{up,[{<<"protocol">>,<<"epmd">>},
                                 {<<"version">>,<<"0">>},
                                 {<<"node">>,<<"node1@host1">>},
                                 {<<"port">>,<<"64249">>}]}},
               {node2@host2,{up,[{<<"protocol">>,<<"epmd">>},
                                 {<<"version">>,<<"0">>},
                                 {<<"node">>,<<"node2@host2">>},
                                 {<<"port">>,<<"64255">>}]}},
               {node3@host3,{up,[{<<"protocol">>,<<"epmd">>},
                                 {<<"version">>,<<"0">>},
                                 {<<"node">>,<<"node3@host3">>},
                                 {<<"port">>,<<"64265">>}]}}]}]
```