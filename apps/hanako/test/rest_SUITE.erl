-module(rest_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([create_board/1, get_board/1, list_boards/1]).
-define(JSON, {<<"content-type">>, "application/json"}).

all() -> [create_board, get_board, list_boards].

init_per_suite(Config) ->
    Port = 61921,
    ok = hanako_storage:create_db(),
    ok = application:set_env(hanako, http_port, Port, [{persistent, true}]),
    ok = application:set_env(hanako, handlers, [hanako_rest_board], [{persistent, true}]),
    {ok, _} = application:ensure_all_started(hanako),

    {ok, _} = application:ensure_all_started(gun),

    [{http_port, Port} | Config].

end_per_suite(_Config) ->
    ok = application:stop(hanako),
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]).

connect(Config) ->
    Port = ?config(http_port, Config),
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),
    ConnPid.

create_board(Config) ->
    Payload = jsx:encode(#{
                <<"name">> => <<"t">>,
                <<"fullname">> => <<"test">>
               }),

    ConnPid = connect(Config),
    StreamRef = gun:post(ConnPid, "/boards", [?JSON], Payload),
    {response, fin, 303, ResHeaders} = gun:await(ConnPid, StreamRef),
    gun:shutdown(ConnPid),

    #{<<"location">> := BoardUrl} = maps:from_list(ResHeaders),
    {save_config, [{board_name, <<"t">>},
                   {board_fullname, <<"test">>},
                   {board_url, BoardUrl}]}.

get_board(Config) ->
    {_, PrevConf} = ?config(saved_config, Config),
    BoardUrl = ?config(board_url, PrevConf),
    BoardName = ?config(board_name, PrevConf),
    BoardFullName = ?config(board_fullname, PrevConf),

    ConnPid = connect(Config),
    StreamRef = gun:get(ConnPid, BoardUrl, [?JSON]),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    gun:shutdown(ConnPid),

    BoardInfo = jsx:decode(Body, [return_maps]),
    #{<<"name">> := BoardName, <<"fullname">> := BoardFullName} = BoardInfo,
    {save_config, PrevConf}.

list_boards(Config) ->
    {_, PrevConf} = ?config(saved_config, Config),
    BoardName = ?config(board_name, PrevConf),

    ConnPid = connect(Config),
    StreamRef = gun:get(ConnPid, "/boards", [?JSON]),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    gun:shutdown(ConnPid),

    [#{<<"name">> := BoardName}] = jsx:decode(Body, [return_maps]),
    {save_config, PrevConf}.
