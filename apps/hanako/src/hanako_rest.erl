-module(hanako_rest).
-export([allowed_methods/3, parse_id/1]).

allowed_methods(Methods, Req, State) ->
    AllMethods = [options, head] ++ Methods,
    TranslatedNames = lists:map(fun atom_to_upper_bitstring/1, AllMethods),
    {TranslatedNames, Req, State}.

atom_to_upper_bitstring(Atom) ->
    list_to_bitstring(string:to_upper(atom_to_list(Atom))).

parse_id(StrId) ->
    try list_to_integer(StrId) of
        NumId -> NumId
    catch
        error:badarg -> list_to_bitstring(StrId)
    end.
