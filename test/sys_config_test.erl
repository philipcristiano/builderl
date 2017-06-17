-module(sys_config_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


make_s3(Key) ->
    [{key, Key}].

validate_syntax_for_package_sys_config_test() ->
    {ok, _Terms} = file:consult("rel/sys.config").
