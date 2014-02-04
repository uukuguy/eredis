gandalf
=======

A Redis-like K/V database using leveldb as storage engine. Features include high performance, high concurrency, scalable etc.

$ rebar create template=riak_kv appid=gandalf nodeid=gandalf force=1

$ make deps

$ make rel

$ make devrel

$ ./startcluster.sh
$ ./joincluster.sh

$ ./dev/dev1/bin/gandalf attach
> gandalf:ping().
> pong

$ rel/gandalf/bin/gandalf console
$ curl -i -H "Accept: application/octet-stream" http://localhost:19090

$ tail -f rel/gandalf/log/gandalf.log

Upload
$ curl -i -T datafile --header "Content-Type: application/octet-stream" http://localhost:18080/b1/f2/d3

Download
$ curl http://localhost:19090/b1/f2/d3 > result.dat

Delete
$ curo -i -X DELETE http://localhost:19090/b1/f2/d3

2014-02-01 01:15:48.144 [error] <0.210.0> riak_kv_env: Open file limit of 1024 is low, at least 4096 is recommended
2014-02-01 01:15:48.145 [error] <0.210.0> riak_kv_env: Cores are disabled, this may hinder debugging
2014-02-01 01:15:48.145 [error] <0.210.0> riak_kv_env: ETS table count limit of 1400 is low, at least 256000 is recommended.
2014-02-01 01:15:48.146 [error] <0.210.0> riak_kv_env: sysctl vm.swappiness is 60, should be no more than 0)
2014-02-01 01:15:48.146 [error] <0.210.0> riak_kv_env: sysctl net.core.wmem_default is 212992, should be at least 8388608)
2014-02-01 01:15:48.146 [error] <0.210.0> riak_kv_env: sysctl net.core.rmem_default is 212992, should be at least 8388608)
2014-02-01 01:15:48.146 [error] <0.210.0> riak_kv_env: sysctl net.core.wmem_max is 212992, should be at least 8388608)
2014-02-01 01:15:48.147 [error] <0.210.0> riak_kv_env: sysctl net.core.rmem_max is 212992, should be at least 8388608)
2014-02-01 01:15:48.147 [error] <0.210.0> riak_kv_env: sysctl net.core.netdev_max_backlog is 1000, should be at least 10000)
2014-02-01 01:15:48.147 [error] <0.210.0> riak_kv_env: sysctl net.core.somaxconn is 128, should be at least 4000)
2014-02-01 01:15:48.147 [error] <0.210.0> riak_kv_env: sysctl net.ipv4.tcp_max_syn_backlog is 128, should be at least 40000)
2014-02-01 01:15:48.147 [error] <0.210.0> riak_kv_env: sysctl net.ipv4.tcp_fin_timeout is 60, should be no more than 15)
2014-02-01 01:15:48.148 [error] <0.210.0> riak_kv_env: sysctl net.ipv4.tcp_tw_reuse is 0, should be 1)
