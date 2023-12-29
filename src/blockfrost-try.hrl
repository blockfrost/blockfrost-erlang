-record(testr, {
  str :: string(),
  may :: string() | undefined,
  wtf :: pls,
  uni :: integer() | string(),
  tup :: {integer(), string()}
}).

-type testr() :: #testr{}.
