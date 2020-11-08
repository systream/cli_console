%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, Systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(parse_test).
-author("Peter Tihanyi").

-include_lib("eunit/include/eunit.hrl").

parse_no_args_test() ->
  assert(["test"], [], "test"),
  assert(["test", "foo"], [], "test foo  "),
  assert(["test", "foo", "bar1231asd"], [], "test foo        bar1231asd"),
  assert(["asd", "@#$!@"], [], "  asd @#$!@").


parse_with_args_test() ->
  assert_args([{"v", true}], "test -v"),
  assert_args([{"v", true}], "tes-t -v"),
  assert_args([{"v", true}], "te=s-t -v"),
  assert_args([{"v", true}], "te=st -v"),
  assert_args([{"v", "1"}], "test -v 1"),
  assert_args([{"v", "1"}], "test -v=1"),
  assert_args([{"v", true}], "test --v"),
  assert_args([{"v", "1"}], "test --v 1"),
  assert_args([{"v", "1"}], "test --v=1"),

  assert_args([{"v", "1"}, {"v", "2"}], "test --v=1 -v 2"),

  assert_args([{"v", "1"}], "test -v\=1"),
  assert_args([{"v", "1"}], "test \\ -v\=1"),
  assert_args([{"v", "1"}], "test \\r\\n -v=1 " ),

  assert_args([{"v", "1"}], "test --v\=1"),
  assert_args([{"v", "1"}], "test \\ --v\=1"),
  assert_args([{"v", "1"}], "test \\r\\n --v=1 "),

  assert_args([{"v", "123-12=3"}], "test --v 123-12=3 " ),
  assert_args([{"v", "123-12=3"}], "test --v=123-12=3 " ),
  ok.

assert(Commands, Flags, Input) ->
  ?assertEqual({ok, Commands, Flags}, cli_console_parser:parse(Input)),
  ?assertEqual({ok, Commands, Flags},
               cli_console_parser:parse(list_to_binary(Input))).

assert_args(Flags, Input) ->
  ?assertMatch({ok, _, Flags}, cli_console_parser:parse(Input)),
  ?assertMatch({ok, _, Flags}, cli_console_parser:parse(list_to_binary(Input))).
