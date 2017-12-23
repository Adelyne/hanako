-module(hanako).
-behaviour(application).
-export([start/2, stop/1, start_phase/3, config_change/3]).

%% @todo configuration of the application
start(_StartType, _Arguments) ->
    ok = hanako_storage:create_tables(),
    hanako_sup:start_link().

stop(_State) -> ok.

start_phase(_Phase, _StartType, _PhaseArgs) -> ok.

config_change(_Changed, _New, _Removed) -> ok.
