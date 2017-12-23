-module(hanako_rest_board).
-include("hanako_model.hrl").
-export([routes/0, init/2, allowed_methods/2, content_types_provided/2,
         resource_exists/2, content_types_accepted/2]).
-export([from_json/2, to_json/2]).
%% @todo authentication
%% @todo patch -> edit
%% @todo html
%% @todo deletion

routes() ->
    [{'_', [
            {"/boards", ?MODULE, #{type => collection}},
            {"/board/:boardid", ?MODULE, #{type => single}}
           ]}].

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, #{type := Type} = State) ->
    Methods = case Type of
                  collection -> [get, post];
                  single -> [get, patch, delete]
              end,
    hanako_rest:allowed_methods(Methods, Req, State).

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, to_json}
%      {{<<"text">>, <<"html">>, []}, to_html}
     ], Req, State}.

resource_exists(Req, #{type := collection} = State) -> {true, Req, State};

resource_exists(Req, #{type := single} = State) ->
    Param = cowboy_req:binding(boardid, Req),
    BoardId = binary_to_integer(Param),
    case hanako_model:board_info(BoardId) of
        {ok, BoardInfo} ->
            {true, Req, State#{board_info => BoardInfo}};
        {error, not_found} ->
            {false, Req, State}
    end.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, from_json}
%      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, from_html}
     ], Req, State}.

from_json(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),
    Name = maps:get(<<"fullname">>, Data),
    Short = maps:get(<<"name">>, Data),
    {ok, Id} = hanako_model:board_add(Name, Short),
    TxtId = integer_to_binary(Id),
    Url = cowboy_req:uri(Req, #{path => <<"/board/", TxtId/binary>>,
                                qs => undefined}),
    {{true, Url}, Req, State}.

to_json(Req, #{type := collection} = State) ->
    {ok, List} = hanako_model:board_list(),
    Prepared = lists:map(fun(Info) -> board_info_to_map(Info) end, List),
    Json = jsx:encode(Prepared),
    {Json, Req, State};

to_json(Req, #{board_info := BoardInfo} = State) ->
    Json = jsx:encode(board_info_to_map(BoardInfo)),
    {Json, Req, State}.

%% @private
board_info_to_map(BoardInfo) ->
    #{
      <<"id">> => BoardInfo#hanako_board.id,
      <<"name">> => BoardInfo#hanako_board.short,
      <<"fullname">> => BoardInfo#hanako_board.name
     }.
