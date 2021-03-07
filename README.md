# CauDEr

A Causal-Consistent Reversible Debugger for Erlang

![CauDEr screenshot](screenshot.png)

**_CAUTION: This tool is still under development_**

## Dependencies

* Erlang 23 or higher

## Building

To build the project, type:

    ./rebar3 compile

To create an _escript_, type:

    ./rebar3 escriptize

To create a release for your platform, type:

    ./rebar3 reltool

To run tests, type:

    ./rebar3 do eunit, ct

To run dialyzer, type:

    ./rebar3 dialyzer

To clean-up the build files, type:

    ./rebar3 clean

## Running

### Using the Erlang shell

To start an Erlang shell with all the required dependencies, type:

    ./rebar3 shell

There are multiples ways to run CauDEr from the Erlang shell:

#### Like an _escript_

```
Eshell V11.0  (abort with ^G)
1> cauder:main().
ok
```

ℹ️ This function will wait for the CauDEr window to close before returning,
which means the shell will be blocked.

#### Like an application

```
Eshell V11.0  (abort with ^G)
1> application:start(wx), application:start(cauder).
ok
```

ℹ️ To stop CauDEr you can use `application:stop(cauder)`, or simply close the
window.

#### Manually

```
Eshell V11.0  (abort with ^G)
1> cauder:start(). % Starting the debugger
{ok,<0.80.0>}
2> cauder_wx:start(). % Starting the GUI
{ok,<0.82.0>,{wx_ref,35,wxFrame,<0.82.0>}}
```

⚠️ If you try to start the GUI without previously starting the debugger, it will
fail with the following error: `{error,{not_started,cauder}}`


### Using the _escript_

    ./_build/default/bin/cauder

ℹ️ This will block the current shell until the CauDEr window is closed.

### Using the release

    ./_build/default/reltool/cauder

ℹ️ This script will start CauDEr in detached mode.

## Documentation

To learn how to use CauDEr you can check the
[Wiki](https://github.com/mistupv/cauder-v2/wiki) (Under construction!)

## License

This project is available under the terms of the MIT license. See the
[`LICENSE`](LICENSE) file for the copyright information and licensing terms.
