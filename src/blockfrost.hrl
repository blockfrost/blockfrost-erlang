-record(error, {
  error :: string(),
  message :: string(),
  status_code :: integer()
}).

-type error() :: #error{}.
