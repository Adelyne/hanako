-module(application_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([startup/1, get_root/1]).

all() -> [startup, get_root].

init_per_suite(Config) ->
    Port = 23288,
    ok = hanako_storage:create_db(),
    ok = application:set_env(hanako, http_port, Port, [{persistent, true}]),
    ok = application:set_env(hanako, handlers, [], [{persistent, true}]),
    {ok, _} = application:ensure_all_started(gun),
    [{http_port, Port} | Config].

end_per_suite(_Config) ->
    ok = application:stop(hanako),
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]).

startup(_Config) ->
    {ok, _} = application:ensure_all_started(hanako).

get_root(Config) ->
    Port = ?config(http_port, Config),
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, "/"),
    {response, _, _, _} = gun:await(ConnPid, StreamRef),
    ok = gun:cancel(ConnPid, StreamRef),
    ok = gun:shutdown(ConnPid).
