Redis Service Discovery using Erlang epmd
=========================================

[![Build Status](https://travis-ci.org/redis-sd/erlang-redis_sd_epmd.png?branch=master)](https://travis-ci.org/redis-sd/erlang-redis_sd_epmd)
[![Build Status](https://drone.io/github.com/redis-sd/erlang-redis_sd_epmd/status.png)](https://drone.io/github.com/redis-sd/erlang-redis_sd_epmd/latest)

Configuration may be specified manually to `redis_sd_epmd:new_service/1` and `redis_sd_epmd:new_browse/1` or through `{browses, BrowseConfigs}` and `{services, ServiceConfigs}` env variables for `redis_sd_epmd`.

Here is a brief example of starting up 2 erlang nodes on the same host with different node and host names:

*`node1@host1`*

```erl
$ erl -pa deps/*/ebin ebin -sname node1@host1 -run redis_sd_epmd manual_start
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false] [dtrace]

Eshell V5.10.3  (abort with ^G)
1> ServiceConfig = [
1>     {name, test_service},
1>     {service, "test-service"},
1>     {ttl, 10}
1> ],
1> redis_sd_epmd:new_service(ServiceConfig).
{ok,<0.65.0>}
2> redis_sd_epmd:nodes().
[]
3> BrowseConfig = [
3>     {service, "test-service"}
3> ],
3> redis_sd_epmd:new_browse(BrowseConfig).
{ok,<0.72.0>}
4> redis_sd_epmd:nodes().
[{node1@host1,true}]
```

*`node2@host2`*

```erl
$ erl -pa deps/*/ebin ebin -sname node2@host2 -run redis_sd_epmd manual_start
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false] [dtrace]

Eshell V5.10.3  (abort with ^G)
1> ServiceConfig = [
1>     {name, test_service},
1>     {service, "test-service"},
1>     {ttl, 10}
1> ],
1> redis_sd_epmd:new_service(ServiceConfig).
{ok,<0.65.0>}
2> redis_sd_epmd:nodes().
[]
3> BrowseConfig = [
3>     {service, "test-service"}
3> ],
3> redis_sd_epmd:new_browse(BrowseConfig).
{ok,<0.75.0>}
4> redis_sd_epmd:nodes().
[{node1@host1,true},{node2@host2,true}]
```
