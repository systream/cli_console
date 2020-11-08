%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, Systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_formatter_test).
-author("Peter Tihanyi").

-include_lib("eunit/include/eunit.hrl").

text_test() ->
  assert("test~n", fun() -> cli_console_formatter:text("test") end).

title_test() ->
  assert("\e[37;1mTitle~n\e[0m", fun() -> cli_console_formatter:title("title") end).

alert_test() ->
  assert( "\e[31;1malert~n\e[0m", fun() -> cli_console_formatter:alert("alert")
                                  end).

warning_test() ->
  assert("\e[33;1mwarning~n\e[0m", fun() ->
                                     cli_console_formatter:warning("warning")
                                   end).

list_test() ->
  assert(" * a~n * b~n * aaaaaaaaaasd3 34123 1234123412 4123412341234123412 34 234123412~n   2341241234123412341234134 123412 124 12~n * d~n",
         fun() ->
           cli_console_formatter:list(
             ["a",
              "b",
              "aaaaaaaaaasd3 34123 1234123412 4123412341234123412 34 234123412341241234123412341234134 123412 124 12",
              "d"
           ])
         end).

table_test() ->
  assert(" ---------------------- ~n | \e[37m a  \e[0m | \e[37m c  \e[0m | \e[37m e  \e[0m |~n ---------------------- ~n |  b   |  d   |      |~n |  b   |      |  f   |~n ---------------------- ~n",
         fun() -> cli_console_formatter:table([#{"a" => "b", "c" => "d"},
                                               #{"a" => "b", "e" => "f"}
                                               ])
                    end).

separator_test() ->
  assert("---------------------------------------------------------------~n",
         fun() -> cli_console_formatter:separator() end).


error_test() ->
  assert("\e[31;1mfatal error~n\e[0m",
         fun() -> cli_console_formatter:error("fatal error") end).

success_test() ->
  assert("\e[32;1mgood~n\e[0m",
         fun() -> cli_console_formatter:success("good") end).


assert(Expected, Fun) ->
  {format, cli_console_formatter, Data} = Fun(),
  ?assertEqual(Expected, cli_console_formatter:format(Data)).
