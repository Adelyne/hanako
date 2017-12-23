-module(application_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([startup/1]).

all() -> [startup].

init_per_suite(Config) ->
    ok = hanako_storage:create_db(),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(hanako),
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]).

startup(_Config) ->
    {ok, _} = application:ensure_all_started(hanako).
