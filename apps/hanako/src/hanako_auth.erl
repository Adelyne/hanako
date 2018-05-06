-module(hanako_auth).
-include("hanako_model.hrl").
-export([user/3, user_pw_ok/2]).

%% @doc Creates a user and hash their password
user(Name, Password, Type) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Hash} = bcrypt:hashpw(Password, Salt),
    #hanako_user{name=Name, hash=Hash, type=Type}.

%% @doc Checks a user's password.
%% @returns A boolean.
user_pw_ok(#hanako_user{hash=Hash}, Password) ->
    {ok, Hash} =:= bcrypt:hashpw(Password, Hash).
