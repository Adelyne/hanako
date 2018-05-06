-module(auth_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("hanako_model.hrl").

setup() ->
    % bcrypt requires at least 4 rounds
    ok = application:set_env(bcrypt, default_log_rounds, 4, [{persistent, true}]),
    {ok, _} = application:ensure_all_started(bcrypt).

user_creation_comparison_test_() ->
    {"Users are created correctly and passwords need to be checked with a specific function.",
     {setup, fun setup/0,
      fun (_) -> {inparallel,
                  [fun compare_same_user/0, fun compare_diff_users/0,
                  fun actually_hash_password/0, fun check_password/0, fun fail_password/0]
                 }
      end}}.

actually_hash_password() ->
    User = hanako_auth:user(undefined, <<"password">>, volatile),
    ?assertNotEqual(User#hanako_user.hash, <<"password">>).

compare_same_user() ->
    % The hashing function should produce a different hash with each run.
    User1 = hanako_auth:user(undefined, <<"password">>, volatile),
    User2 = hanako_auth:user(undefined, <<"password">>, volatile),
    ?assertNotEqual(User1, User2).

compare_diff_users() ->
    User1 = hanako_auth:user(undefined, <<"password1">>, volatile),
    User2 = hanako_auth:user(undefined, <<"password2">>, volatile),
    ?assertNotEqual(User1, User2).

check_password() ->
    User = hanako_auth:user(undefined, <<"password">>, volatile),
    ?assert(hanako_auth:user_pw_ok(User, <<"password">>)).

fail_password() ->
    User = hanako_auth:user(undefined, <<"correctpassword">>, volatile),
    ?assertNot(hanako_auth:user_pw_ok(User, <<"wrongpassword">>)).
