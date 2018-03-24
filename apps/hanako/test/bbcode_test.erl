-module(bbcode_test).
-include_lib("eunit/include/eunit.hrl").

%% ---------------- %%
%% Helper functions %%
%% ---------------- %%
parse(Text) ->
    bbcode:clean(bbcode:parse(Text)).

parse(Text, MaxDepth) ->
    bbcode:clean(bbcode:parse(Text, MaxDepth)).

%% ----------- %%
%% Basic cases %%
%% ----------- %%
raw_test() ->
    Text = <<"test">>,
    ?assertEqual([Text], parse(Text)).

empty_test() ->
    ?assertEqual([], parse(<<"">>)).

simple_emph_test() ->
    Text = <<"this [i]is[/i] a [i]test[/i]">>,
    Expected = [
                <<"this ">>,
                {emph, <<"is">>},
                <<" a ">>,
                {emph, <<"test">>}
               ],
    ?assertEqual(Expected, parse(Text)).

empty_emph_test() ->
    Text = <<"this [i]is[/i] [i][/i]a [i]test[/i]">>,
    Expected = [
                <<"this ">>,
                {emph, <<"is">>},
                <<" ">>,
                <<"a ">>,
                {emph, <<"test">>}
               ],
    ?assertEqual(Expected, parse(Text)).

%% ------- %%
%% Nesting %%
%% ------- %%
nested_emph_test() ->
    Text = <<"this [i]is [i]yet[/i] another [i]test[/i][/i] case">>,
    Expected = [
                <<"this ">>,
                {emph, [
                        <<"is ">>,
                        {emph, <<"yet">>},
                        <<" another ">>,
                        {emph, <<"test">>}
                       ]},
                <<" case">>
               ],
    ?assertEqual(Expected, parse(Text)).

doubled_tag_test() ->
    Text = <<"this is [i][i]yet[/i][/i] another test case">>,
    Expected = [
                <<"this is ">>,
                {emph, [
                        {emph, <<"yet">>}
                       ]},
                <<" another test case">>
               ],
    ?assertEqual(Expected, parse(Text)).

%% ---------- %%
%% Exceptions %%
%% ---------- %%
unbalanced_test() ->
    Text = <<"this [i]is [i]left[/i] open">>,
    ?assertThrow({unclosed_tag, {opening_tag, emph}, _State}, parse(Text)).

wrong_tag_test() ->
    Text = <<"this [i]is [b]not[/i] [/i] correct">>,
    ?assertThrow({wrong_closing_tag,
                  {opening_tag, strong},
                  {closing_tag, emph},
                  _State},
                 parse(Text)).

max_nesting_depth_test() ->
    Text = <<"this [i]is [i]yet[/i] another [i]test[/i][/i] case">>,
    ?assertThrow({max_nesting_depth_exceeded, 1, _State}, parse(Text, 1)).

%% --------------- %%
%% HTML generation %%
%% --------------- %%
html_test() ->
    Text = <<"this [i]is[/i] a [b]test[/b]">>,
    Expected = <<"this <em>is</em> a <strong>test</strong>">>,
    ?assertEqual(Expected, bbcode:to_html(parse(Text))).
