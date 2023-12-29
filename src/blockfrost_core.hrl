-type network() ::
     mainnet
   | preprod
   | preview
   | sanchonet
   | ipfs
   .

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

-define(max_page_size, 100).

-type sort_order() :: asc | desc.

-define(domain, "blockfrost.io").

% -define(debug, true).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.
