-record(error, {
  error :: string(),
  message :: string(),
  status_code :: integer()
}).

-type error() :: #error{}.

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.
