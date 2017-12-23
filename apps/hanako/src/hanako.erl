-module(hanako).
-behaviour(application).
-export([start/2, stop/1, start_phase/3, config_change/3]).

start(_StartType, _StartArgs) ->
    ok = hanako_storage:create_tables(),
    Opts = maps:from_list(application:get_all_env()),
    ok = start_web(Opts),
    hanako_sup:start_link().

%% @private
start_web(Opts) ->
    Defaults = #{
      http_port => undefined,
      https_port => undefined,
      handlers => []
    },
    #{
       http_port := HttpPort,
       https_port := _HttpsPort,
       handlers := Handlers
      } = maps:merge(Defaults, Opts),
    Routes = lists:flatmap(fun(Module) -> Module:routes() end, Handlers),
    CompiledRoutes = cowboy_router:compile(Routes),
    ProtocolOpts = #{env => #{dispatch => CompiledRoutes}},
    if
        is_integer(HttpPort) ->
           {ok, _} = cowboy:start_clear(hanako_http, [{port, HttpPort}], ProtocolOpts),
           ok;
        HttpPort =:= undefined ->
            ok
    end.

stop(_State) -> ok.

start_phase(_Phase, _StartType, _PhaseArgs) -> ok.

config_change(_Changed, _New, _Removed) -> ok.
