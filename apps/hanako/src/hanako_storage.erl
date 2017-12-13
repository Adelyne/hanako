-module(hanako_storage).
-export([start/2, stop/0, clean/0]).
-include("hanako_model.hrl").
-define(CREATE_TABLE(Name), create_table(Name, record_info(fields, Name))).

start(Dir, DumpLogTimeout) ->
    ok = init_db(Dir, DumpLogTimeout),
    ok = application:start(mnesia),
    ok = ?CREATE_TABLE(hanako_board),
    ok = ?CREATE_TABLE(hanako_thread),
    ok = ?CREATE_TABLE(hanako_post),
    ok = ?CREATE_TABLE(hanako_user),
    ok.

stop() ->
    ok = application:stop(mnesia).

clean() ->
    ok = mnesia:delete_schema([node()]).

%% @private
init_db(Dir, DumpLogTimeout) ->
    application:set_env(mnesia, dump_log_time_threshold, DumpLogTimeout),
    application:set_env(mnesia, dir, Dir),
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
