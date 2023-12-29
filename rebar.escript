#!/usr/bin/env escript
main(_) ->
    code:load_abs(filename:join(os:getenv("HOME"), ".erlang.d/user_default")),
    % load our local user_default(s) so we have records in shell
    compile:file("user_default.erl", [{warn_format, 0}]),
    code:load_abs("user_default").
