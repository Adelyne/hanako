-module(hanako_storage).
-include("hanako_model.hrl").
-export([create_db/0, create_tables/0]).
-define(CREATE_TABLE(Name), create_table(Name, record_info(fields, Name))).

create_tables() ->
    ok = ?CREATE_TABLE(hanako_board),
    ok = ?CREATE_TABLE(hanako_thread),
    ok = ?CREATE_TABLE(hanako_post),
    ok = ?CREATE_TABLE(hanako_user).

create_db() ->
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @private
create_table(Name, Attributes) ->
    Options = [{attributes, Attributes},
               {disc_copies, [node()]}],
    case mnesia:create_table(Name, Options) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _Name}} -> ok;
        {aborted, Reason} -> {error, {create_table, Name}, Reason}
    end.
