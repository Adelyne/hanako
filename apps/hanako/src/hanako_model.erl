-module(hanako_model).
-include_lib("stdlib/include/qlc.hrl").
-include("hanako_model.hrl").
-export([board_add/2, board_delete/1, board_info/1, board_list/0]).
-export([thread_add/3, thread_delete/1, thread_list/1]).
-export([post_add/1, post_delete/1, post_list/1, post_from_ip/1]).
-export_type([basic_id/0, modifier/0, ip_or_subnet/0]).


%% ------ %%
%% Boards %%
%% ------ %%

-spec board_add(iolist(), iolist()) -> transaction(basic_id()).
board_add(Name, Shorthand) ->
    Op = fun() ->
                 Query = qlc:q([Id || #hanako_board{id=Id}
                                      <- mnesia:table(hanako_board)]),
                 PrevId = qlc:fold(fun(Id, Max) ->
                                           if Id > Max -> Id;
                                              Id =< Max -> Max
                                           end
                                   end, 0, Query),
                 Id = PrevId + 1,
                 mnesia:write(#hanako_board{
                                 id=Id,
                                 name=Name,
                                 short=Shorthand
                                }),
                 Id
         end,
    transaction(Op).

-spec board_delete(basic_id()) -> ok | {error, term()}.
%% @todo
board_delete(_Id) -> ok.

-spec board_info(basic_id() | bitstring()) ->
    transaction(single_result(#hanako_board{})).
board_info(Id) when is_integer(Id) ->
    Op = fun() ->
                 single_result(mnesia:read(hanako_board, Id))
         end,
    transaction(Op);

board_info(ShortName) when is_bitstring(ShortName) ->
    Op = fun() ->
                 Q = qlc:q([Board || Board <- mnesia:table(hanako_board),
                                     Board#hanako_board.short =:= ShortName]),
                 single_result(qlc:eval(Q))
         end,
    transaction(Op).

-spec board_list() -> transaction([] | [#hanako_board{}]).
%% @doc Return all available boards without sorting.
board_list() ->
    Op = fun() ->
                 Query = qlc:q([Board || Board <- mnesia:table(hanako_board)]),
                 qlc:eval(Query)
         end,
    transaction(Op).


%% ------- %%
%% Threads %%
%% ------- %%
%% @todo thread edition

-spec thread_add(
        #metadata{},
        ThreadInfo::#{
          board_id := basic_id(),
          subject => iolist(),
          modifier => [modifier()]
         },
        PostInfo::#{
          text => txt(),
          quotes => [basic_id()],
          media => #media{}
         }) ->
    transaction(#{
      thread_id := #id{},
      post_id := #id{}
     }).
thread_add(Metadata, ThreadInfo, PostInfo) ->
    BoardId = maps:get(board_id, ThreadInfo),
    Subject = maps:get(subject, ThreadInfo, undefined),
    Modifier = maps:get(modifier, ThreadInfo, []),

    Op = fun() ->
                 SubId = increment_counter(BoardId),
                 ThreadId = #id{id=SubId, board=BoardId},
                 Thread = #hanako_thread{
                             id=ThreadId,
                             metadata=Metadata,
                             subject=Subject,
                             modifier=Modifier
                            },
                 ok = mnesia:write(Thread),
                 PostId = post_add_with_id(#{
                            post_sub_id => SubId,
                            thread_id => ThreadId,
                            metadata => Metadata,
                            text => maps:get(text, PostInfo, []),
                            quotes => maps:get(quotes, PostInfo, []),
                            media => maps:get(media, PostInfo, undefined)
                           }),
                 #{ thread_id => ThreadId, post_id => PostId }
         end,
    transaction(Op).

-spec thread_delete(basic_id()) -> ok | {error, term()}.
%% @todo
thread_delete(_Id) -> ok.

-spec thread_list(#{
        board_id := basic_id(),
        limit => pos_integer(),
        skip => pos_integer()
       }) -> transaction([] | [#hanako_thread{}]).
%% @doc Return the posts in a thread. The output is left unsorted.
thread_list(Args) ->
    #{board_id := BoardId} = Args,
    Op = fun() ->
                 Query = qlc:q([Thread || Thread <- mnesia:table(hanako_thread),
                                          Thread#hanako_thread.id#id.board =:= BoardId]),
                 qlc:eval(Query)
         end,
    transaction(Op).


%% ----- %%
%% Posts %%
%% ----- %%
%% @todo post edition

-spec post_add_with_id(#{
        post_sub_id := basic_id(),
        thread_id := #id{},
        metadata := #metadata{},
        text => txt(),
        quotes => [#id{}],
        media => #media{}
       }) -> #id{}.
%% @private
%% @doc Create a post by providing its sub-ID.
post_add_with_id(Info) ->
    SubId = maps:get(post_sub_id, Info),
    ThreadId = maps:get(thread_id, Info),
    BoardId = ThreadId#id.board,
    Id = #id{id=SubId, board=BoardId},

    Metadata = maps:get(metadata, Info),
    Text = maps:get(text, Info, []),
    Quotes = maps:get(quotes, Info, []),
    Media = maps:get(media, Info, undefined),

    ok = mnesia:write(#hanako_post{
                         id=Id,
                         thread=ThreadId,
                         metadata=Metadata,
                         text=Text,
                         quotes=Quotes,
                         media=Media
                        }),
    Id.

-spec post_add(#{
        thread_id := #id{},
        metadata := #metadata{},
        text => txt(),
        quotes => [#id{}],
        media => #media{}
       }) -> transaction(#id{}).
%% @todo check that ThreadId exists
post_add(Info) ->
    Op = fun() ->
                 ThreadId = maps:get(thread_id, Info),
                 BoardId = ThreadId#id.board,
                 PostSubId = increment_counter(BoardId),
                 post_add_with_id(maps:put(post_sub_id, PostSubId, Info))
         end,
    transaction(Op).


-spec post_delete(#id{}) -> ok | {error, term()}.
%% @todo what about posts that quote the deleted post?
post_delete(_PostId) -> ok.

-spec post_list(#id{}) -> transaction([] | [#hanako_post{}]).
%% @doc Return all posts in a thread. The output is left unsorted.
post_list(ThreadId) ->
    Op = fun() ->
                 Query = qlc:q([Post || Post <- mnesia:table(hanako_post),
                                        Post#hanako_post.id =:= ThreadId]),
                 qlc:eval(Query)
         end,
    transaction(Op).


-spec post_from_ip(ip_or_subnet()) -> transaction([] | [#hanako_post{}]).
%% @todo subnets
post_from_ip(IP) ->
    Op = fun() ->
                 Query = qlc:q([Post || Post <- mnesia:table(hanako_post),
                                        has_ip(Post, IP)]),
                 qlc:eval(Query)
         end,
    transaction(Op).


%% ----- %%
%% Utils %%
%% ----- %%
-type transaction(Result) :: {ok, Result} | {error, term()}.
%% @private
%% @doc wrapper for getting backend-agnostic tuples out of mnesia transactions
transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, not_found} -> {error, not_found};
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.

-type single_result(Result) :: not_found | Result.
%% @private
single_result([]) -> not_found;
single_result([Result]) -> Result.

%% @private
%% @doc for use in transactions
increment_counter(BoardId) ->
    [Board] = mnesia:read(hanako_board, BoardId),
    Counter = Board#hanako_board.counter,
    Counter + 1.

%% @private
%% @todo implement subnet detection
has_ip(Post, IP) ->
    Metadata = Post#hanako_post.metadata,
    ThisIP = Metadata#metadata.ip,
    IP =:= ThisIP.
