%% --------------------- %%
%% Basic data structures %%
%% --------------------- %%
-type basic_id() :: non_neg_integer().
-type modifier() :: locked | pinned.
-type ip_or_subnet() :: inet:ip_address() | {inet:ip_address(), pos_integer()}.
-type txt() :: bbcode:bbtree().

-record(hanako_user, {
          name=undefined :: undefined | bitstring(), % must be unique if not undefined
          hash :: binary(), % password hash or tripcode hash
          type=volatile :: admin | moderator | volatile % if volatile and name =:= <<"">> => default name
         }).

-record(metadata, {
          created :: erlang:timestamp(),
          deleted=undefined :: undefined | erlang:timestamp(),
          bump=true :: true | false,
          ip :: inet:ip_adress(),
          user :: #hanako_user{} % use document embedding as a user can be deleted (esp. if volatile)
         }).

-record(id, {
          id :: basic_id(),
          board :: basic_id()
         }).

-record(media, {
          type :: atom(), % gif, png, webm...
          id :: bitstring(), % filename, youtube id...
          metadata :: term() % depends on the type: size, width/height, youtube channel, duration...
         }).

-record(hanako_board, {
          id :: basic_id(),
          counter=0 :: non_neg_integer(),
          name :: bitstring(),
          short :: bitstring()
         }).

-record(hanako_post, {
          id :: #id{},
          thread :: #id{},
          metadata :: #metadata{},
          text=[] :: txt(), 
          quotes=[] :: [] | [#id{}],
          media=undefined :: undefined | #media{}
         }).

-record(hanako_thread, {
          id :: #id{},
          metadata :: #metadata{},
          subject=undefined :: undefined | bitstring(),
          modifier=[] :: [modifier()]
         }).

-record(hanako_ban, {
          ip :: ip_or_subnet(),
          reason=undefined :: undefined | bitstring(),
          duration=0 :: non_neg_integer(),
          appeal=0 :: non_neg_integer(),
          boards=all :: all | [basic_id()]
         }).
