-record(error, {
  error :: string(),
  message :: string(),
  status_code :: integer()
}).

-type error() :: #error{}.

-record(paged, {
  count_per_page = 100 :: integer(),
  page_number = 1 :: integer()
}).

-type paged() :: #paged{}.

-type sort_order() :: asc | desc.

-define(domain, "blockfrost.io").

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.
