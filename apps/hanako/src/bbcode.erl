%% @doc Module for dealing with BBCode: create a parse tree, convert it to HTML.
%% @todo URL validation, auto-linkification
-module(bbcode).
-export([parse/1, parse/2, clean/1, to_html/1]).
-export_type([bbtree/0]).

-type bbtree() :: [] | [ {atom(), iolist()|bbtree()} | iolist() ].

-spec parse(iolist()) -> bbtree().
%% @see parse/2.
%% @doc Set a default max nesting depth for BBCode tags (10).
parse(Text) -> parse(Text, 10).

-spec parse(iolist(), integer()) -> bbtree() | { [iolist()]|[], bbtree()|iolist(), atom() }.
%% @see parse/2.
%% @doc Turn a bitstring into a parse tree. Each level is a list that can contain:
%%  - bitstrings for text content
%%  - tuples starting with the name of a tag, followed by:
%%    - another tree containing at least one tag tuple
%%    - a bitstring to which the tag applies
%% There are no bitstrings alone in a list besides in the outermost list.
%% @end
%% @throws TODO
parse(Text, MaxDepth) when is_bitstring(Text) ->
    BareTokens = re:split(Text, <<"(\\[/?[a-z]+\\])">>, [trim]),
    Tokens = lists:map(fun name_tags/1, BareTokens),
    parse(Tokens, MaxDepth);

parse(Tokens, MaxDepth) when MaxDepth > 0 ->
    try parse(Tokens, [], MaxDepth) of
        {[], Tree} -> Tree
    catch
        {max_nesting_depth_exceeded, State} ->
            throw({max_nesting_depth_exceeded, MaxDepth, State})
    end.

parse(Tokens, Tree, -1) ->
    throw({max_nesting_depth_exceeded,
           {state,
            {remaining_tokens, Tokens},
            {tree, Tree}
           }});

parse([], Tree, _MaxDepth) -> {[], lists:reverse(Tree)};

parse([{Tag, open}|Tokens], Tree, MaxDepth) ->
    case parse(Tokens, [], MaxDepth - 1) of
        {Remainder, SubTree, Tag} ->
            parse(Remainder, [{Tag, SubTree}|Tree], MaxDepth);
        {Remainder, SubTree, WrongTag} ->
            throw({wrong_closing_tag,
                   {opening_tag, Tag},
                   {closing_tag, WrongTag},
                   {state,
                    {tree, Tree},
                    {sub_tree, SubTree},
                    {remaining_tokens, Remainder}
                   }});
        {Remainder, SubTree} ->
            throw({unclosed_tag,
                   {opening_tag, Tag},
                   {state,
                    {tree, Tree},
                    {sub_tree, SubTree},
                    {remaining_tokens, Remainder}
                   }})
    end;

parse([{Tag, close}|Tokens], [Tree], _MaxDepth) ->
    {Tokens, Tree, Tag};

parse([{Tag, close}|Tokens], Tree, _MaxDepth) ->
    {Tokens, lists:reverse(Tree), Tag};

parse([Text|Tokens], Tree, MaxDepth) ->
    parse(Tokens, [Text|Tree], MaxDepth).


-spec name_tags(iolist()) -> {atom(), atom()}.
%% @private
%% @see parse/2.
%% @todo pass a map of tag => name and detect closing/opening tags
%% instead of hardcoding everything. This would require Erlang/OTP 20
%% as its string manipulation functions are using iolists instead of lists.
name_tags(BareToken) ->
    case BareToken of
        <<"[i]">> -> {emph, open};
        <<"[/i]">> -> {emph, close};
        <<"[b]">> -> {strong, open};
        <<"[/b]">> -> {strong, close};
        <<"[s]">> -> {strike, open};
        <<"[/s]">> -> {strike, close};
        <<"[spoiler]">> -> {spoiler, open};
        <<"[/spoiler]">> -> {spoiler, close};
        <<"[aa]">> -> {ascii_art, open};
        <<"[/aa]">> -> {ascii_art, close};
        Text -> Text
    end.


-spec clean(bbtree()) -> bbtree().
%% @doc Remove empty nodes from an Tree.
%% Especially useful for comparisons with hand-typed Trees in unit tests.
%% @end
clean(Tree) ->
    clean(Tree, []).

clean([], NewTree) ->
    lists:reverse(NewTree);

clean([<<>>|Tree], NewTree) ->
    clean(Tree, NewTree);

clean([{_Tag, <<>>}|Tree], NewTree) ->
    clean(Tree, NewTree);

clean([{Tag, SubTree}|Tree], NewTree) ->
    NewSubTree = clean(SubTree, []),
    clean(Tree, [{Tag, NewSubTree}|NewTree]);

clean([Value|Tree], NewTree) ->
    clean(Tree, [Value|NewTree]);

clean(Value, []) ->
    Value.


-spec to_html(bbtree()) -> bitstring().
%% @doc Serialize an Tree from this module into an HTML bitstring.
to_html(Tree) ->
    to_html(Tree, <<"">>).

to_html([], HTML) -> HTML;

to_html([{Tag, Value}|Tree], HTML) ->
    {Open, Close} = html_tag(Tag),
    InnerHTML = if is_list(Value) ->
                       to_html(Value, <<"">>);
                   is_bitstring(Value) -> Value
                end,
    SubHTML = <<Open/binary, InnerHTML/binary, Close/binary>>,
    to_html(Tree, <<HTML/binary, SubHTML/binary>>);

to_html([Text|Tree], HTML) ->
    to_html(Tree, <<HTML/binary, Text/binary>>).

-spec html_tag(term()) -> {bitstring(), bitstring()}.
%% @private
%% @see to_html/2.
%% @todo Instead of hard-coded values pass a map to to_html/1.
html_tag(Tag) ->
    case Tag of
        emph -> {<<"<emph>">>, <<"</emph>">>};
        strong -> {<<"<strong>">>, <<"</strong>">>};
        strike -> {<<"<span class=\"strike\">">>, <<"</span>">>};
        spoiler -> {<<"<span class=\"spoiler\">">>, <<"</span>">>};
        ascii_art -> {<<"<span class=\"ascii_art\">">>, <<"</span>">>};
        Tag -> throw({unrecognized_tag, Tag})
    end.
