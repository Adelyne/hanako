-module(model_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("hanako_model.hrl").
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([board_create_read/1, thread_create/1, thread_read/1]).
-export([post_create/1, post_read/1, post_from_ip/1]).

all() -> [board_create_read, thread_create, thread_read,
          post_create, post_read, post_from_ip].

init_per_suite(Config) ->
    ok = hanako_storage:create_db(),
    ok = application:start(mnesia),
    ok = hanako_storage:create_tables(),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]).

board_create_read(_Config) ->
    {ok, Id} = hanako_model:board_add(<<"test">>, <<"t">>),
    {ok, Info} = hanako_model:board_info(Id),
    #hanako_board{id=Id, name= <<"test">>, short= <<"t">>} = Info,
    {ok, Info} = hanako_model:board_info(<<"t">>),
    {error, not_found} = hanako_model:board_info(<<"x">>),
    {ok, [Info]} = hanako_model:board_list(),
    {save_config, [{board_id, Id}]}.

thread_create(Config) ->
    {_, PrevConf} = ?config(saved_config, Config),
    BoardId = ?config(board_id, PrevConf),
    Timestamp = os:timestamp(),
    Hash = crypto:hmac(sha, <<"salt">>, <<"tripcode">>),
    User = #hanako_user{hash=Hash},
    IP = inet:parse_address("192.168.0.1"),
    Metadata = #metadata{created=Timestamp, ip=IP, user=User},

    {ok, Result} = hanako_model:thread_add(Metadata, #{ board_id => BoardId }, #{}),
    #{post_id := PostId} = Result,
    #id{board=BoardId} = PostId,
    ThreadId = maps:get(thread_id, Result),
    {save_config, [
                   {thread_id, ThreadId},
                   {metadata, Metadata},
                   {op_id, PostId},
                   {ip, IP}
                  ]}.

%% @todo check skip and limit parameters, and verify it only 
%% returns threads from a single board
thread_read(Config) ->
    {_, PrevConf} = ?config(saved_config, Config),
    ThreadId = ?config(thread_id, PrevConf),
    BoardId = ThreadId#id.id,
    {ok, Result} = hanako_model:thread_list(#{board_id => BoardId}),
    [#hanako_thread{id=ThreadId}] = Result,
    {save_config, PrevConf}.

post_create(Config) ->
    {thread_read, PrevConf} = ?config(saved_config, Config),
    ThreadId = ?config(thread_id, PrevConf),
    Metadata = ?config(metadata, PrevConf),
    {ok, PostId} = hanako_model:post_add(#{
                     thread_id => ThreadId,
                     metadata => Metadata
                    }),
    {save_config, [{reply_id, PostId} | PrevConf]}.


post_read(Config) ->
    {_, PrevConf} = ?config(saved_config, Config),
    ThreadId = ?config(thread_id, PrevConf),
    OPId = ?config(op_id, PrevConf),
    ReplyId = ?config(reply_id, PrevConf),
    {ok, Posts} = hanako_model:post_list(ThreadId),
    Ids = lists:map(fun(Post) -> Post#hanako_post.id end, Posts),
    true = lists:member(OPId, Ids),
    true = lists:member(ReplyId, Ids),
    {save_config, PrevConf}.

%% @todo test subnets
post_from_ip(Config) ->
    {_, PrevConf} = ?config(saved_config, Config),
    ThreadId = ?config(thread_id, PrevConf),
    IP = ?config(ip, PrevConf),
    {ok, Results} = hanako_model:post_from_ip(IP),
    {ok, Posts} = hanako_model:post_list(ThreadId),
    true = lists:all(fun(Post) -> lists:member(Post, Posts) end, Results),
    {save_config, PrevConf}.
