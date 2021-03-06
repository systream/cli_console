Command Line tool
=====

Parsing and executing console commands 

## Example
Until i have time to write proper documentation here is an example how to use it:

### Setup release
Add to your project's relx overlay config:

```erlang
{relx, [
  ...
  {overlay, [{template, "_build/default/lib/cli_console/rel/admin.sh", "bin/admin"}
            ]}
]}.
```

In the release you can use it ex: `./bin/admin help`

### Defining arguments
Available argument types: `flag | atom | string | binary | integer`
Arguments converted to target type automatically. 

Example: 

Simple `flag` argument called `all`, with `List all partitions` description. 
Descriptions are useful when printing automatically generated help.   
```erlang
All = cli_console_command_arg:argument("all", flag, "List all partitions").
```

It is possible to set a default value for an argument, if it is not provided. 
```erlang
Node = cli_console_command_arg:set_default(
        cli_console_command_arg:argument("node", atom, "Target node name"), 
        node()
      ),
```

If an argument is required, it can be marked as a mandatory.
```erlang
Limit = cli_console_command_arg:mandatory(
          cli_console_command_arg:argument("limit", integer, "Max number of items to show")
        ).
```

#### Create command handler fun

When registering a command it is needed to provide a command handler fun. 

Example:
```erlang
list_partitions(Args) ->
  [cli_console_formatter:title("List of partions"),
   cli_console_formatter:separator(),
   cli_console_formatter:text("Node: ~p", [proplists:get_value("node", Args)]),
   cli_console_formatter:text("Limit: ~p", [proplists:get_value("limit", Args)]),
   cli_console_formatter:table(get_partitions(proplists:get_value("all", Args, false)))
  ].

get_partitions(true) ->
  [#{"partition" => [P], "node" => atom_to_list(node())} || P <- lists:seq($a, $z)];
get_partitions(false) ->
  [#{"partition" => [P], "node" => atom_to_list(node())} || P <- lists:seq($a, $d)].
```

#### Registering command

Example: 
```erlang
cli_console:register(["list", "partitions"], [All, Node, Limit], fun list_partitions/1, "List partitions").
```

Or 

```erlang
cli_console:register(["list", "partitions", Node], [All, Limit], funlist_partitions/1, "List partitions").
```

This ad-hoc register command method does not survive if the cli_console service
has restarted due to crash or anything else. Use register command callbacks
instead.
 
#### Register command callbacks
 
There is `cli_console:register/2` function where the module and function parameters
must be given.

First there mush define a function which contains the desired commands.
It should return a list with tuples.
Tuple should look like this (same as `cli_console:register/4` arguments):
`{Commands :: [command()], Arguments::  [command_argument()], command_fun(), Description ::string()}` 
 
Example module for callback:
```erlang
-module(test_module).

%% API
-export([register_cli/0]).

-spec register_cli() -> [].
register_cli() ->
  [  
    {["clustre", "status"], [], fun(_) -> {text, "status: ok"} end, "Listpartitions" }
  ].  
````

Registering callback:
```erlang  
ok = cli_console:register(test_module, register_cli).
```

Cluster status command will survive if cli_console process crashes or restarted. 

#### Run command

Example:
```erlang
cli_console:run("list partitions --limit 123 -node=testnode --all").
```

or 

```erlang
cli_console:run("list partitions testnode --limit 123 --all").
```

Output: 
```bash
List of partions
---------------------------------------------------------------
Node: test
Limit: 123
 ----------------------------------- 
 |       node       |  partition   |
 ----------------------------------- 
 |  nonode@nohost   |      a       |
 |  nonode@nohost   |      b       |
 |  nonode@nohost   |      c       |
 |  nonode@nohost   |      d       |
 |  nonode@nohost   |      e       |
 |  nonode@nohost   |      f       |
 |  nonode@nohost   |      g       |
 |  nonode@nohost   |      h       |
 |  nonode@nohost   |      i       |
 |  nonode@nohost   |      j       |
 |  nonode@nohost   |      k       |
 |  nonode@nohost   |      l       |
 |  nonode@nohost   |      m       |
 |  nonode@nohost   |      n       |
 |  nonode@nohost   |      o       |
 |  nonode@nohost   |      p       |
 |  nonode@nohost   |      q       |
 |  nonode@nohost   |      r       |
 |  nonode@nohost   |      s       |
 |  nonode@nohost   |      t       |
 |  nonode@nohost   |      u       |
 |  nonode@nohost   |      v       |
 |  nonode@nohost   |      w       |
 |  nonode@nohost   |      x       |
 |  nonode@nohost   |      y       |
 |  nonode@nohost   |      z       |
 -----------------------------------
```

Build
-----

    $ rebar3 compile


Test
-----

    $ rebar3 test
        
TODO
-----  
* Implement "Register command" callbacks, to not loose commands when process crashes.
