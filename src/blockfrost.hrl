-record(error, {
  error :: string(),
  message :: string(),
  status_code :: integer()
}).

-type error() :: #error{}.

-record(testr, {
  str :: string(),
  may :: string() | undefined,
  wtf :: pls,
  uni :: integer() | string(),
  tup :: {integer(), string()}
}).

-type testr() :: #testr{}.
