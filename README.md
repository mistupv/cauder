# CauDEr

A Causal-Consistent Reversible Debugger for Erlang.

[![Erlang](https://img.shields.io/badge/Erlang%2FOTP-23.0-blue?logo=erlang)](https://www.erlang.org/)
[![GitHub Actions](https://img.shields.io/github/workflow/status/mistupv/cauder/Test?label=test&logo=github)](https://github.com/mistupv/cauder/actions/workflows/erlang.yml)
[![License](https://img.shields.io/github/license/mistupv/cauder?label=License&logo=data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz48IURPQ1RZUEUgc3ZnIFBVQkxJQyAiLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4iICJodHRwOi8vd3d3LnczLm9yZy9HcmFwaGljcy9TVkcvMS4xL0RURC9zdmcxMS5kdGQiPjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiB3aWR0aD0iMjQiIGhlaWdodD0iMjQiIHZpZXdCb3g9IjAgMCAyNCAyNCI+PHBhdGggZmlsbD0iI0Y1RjVGNSIgZD0iTTEyLDNDMTAuNzMsMyA5LjYsMy44IDkuMTgsNUgzVjdINC45NUwyLDE0QzEuNTMsMTYgMywxNyA1LjUsMTdDOCwxNyA5LjU2LDE2IDksMTRMNi4wNSw3SDkuMTdDOS41LDcuODUgMTAuMTUsOC41IDExLDguODNWMjBIMlYyMkgyMlYyMEgxM1Y4LjgyQzEzLjg1LDguNSAxNC41LDcuODUgMTQuODIsN0gxNy45NUwxNSwxNEMxNC41MywxNiAxNiwxNyAxOC41LDE3QzIxLDE3IDIyLjU2LDE2IDIyLDE0TDE5LjA1LDdIMjFWNUgxNC44M0MxNC40LDMuOCAxMy4yNywzIDEyLDNNMTIsNUExLDEgMCAwLDEgMTMsNkExLDEgMCAwLDEgMTIsN0ExLDEgMCAwLDEgMTEsNkExLDEgMCAwLDEgMTIsNU01LjUsMTAuMjVMNywxNEg0TDUuNSwxMC4yNU0xOC41LDEwLjI1TDIwLDE0SDE3TDE4LjUsMTAuMjVaIiAvPjwvc3ZnPg==)](https://github.com/mistupv/cauder/blob/dev/LICENSE)

**_This tool is still under development_**

### Core Erlang version

In 2020, we decided to rewrite CauDEr to work directly with Erlang instead of
Core Erlang. The main reasons for this change where simplicity,
user-friendliness and breaking changes introduced in newer version of
Erlang/OTP.

The old version of CauDEr developed for Core Erlang is still available at
[mistupv/cauder-core](https://github.com/mistupv/cauder-core), however it is no
longer being actively maintained.

## Dependencies

* Erlang 23 or higher

## Building

To build the project, type:

    ./rebar3 compile

To create an _escript_, type:

    ./rebar3 escriptize

To create a release for your platform, type:

    ./rebar3 reltool

## Reviewing

To run a success typing analysis, type:

    ./rebar3 dialyzer

To run a cross-reference analysis, type:

    ./rebar3 xref

To run the code formatter, type:

    ./rebar3 fmt

To run the style reviewer, type:

    ./rebar3 lint

To run the unit tests, type:

    ./rebar3 eunit

To run the common tests, type:

    ./rebar3 ct

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

In Unix systems a symbolic link pointing to `./_build/default/bin/cauder` will
be created in the root of the project.

### Using the release

    ./_build/default/reltool/cauder

ℹ️ This script will start CauDEr in detached mode.

## Creating a log

To run CauDEr in replay mode you will first need to create a log, for that
purpose you can use the [Tracer](https://github.com/mistupv/tracer) tool.

**NOTE:** You should use the version of tracer from branch
[`with_dbg`](https://github.com/mistupv/tracer/tree/with_dbg) for logs to work
with the current version of CauDEr. 

## Screenshot

![CauDEr screenshot](screenshot.png)

## Documentation

To learn how to use CauDEr you can check the
[Wiki](https://github.com/mistupv/cauder/wiki) (Under construction!)

## Mailing list

Announcements as well as questions and discussion among users and developers of
the causal consistent reversible debugger CauDEr for Erlang programs are carried
out in the [Mailing List](https://listas.upv.es/mailman/listinfo/cauder)

## Contributing

Please read the [Contributing guide](.github/CONTRIBUTING.md).

## License

This project is available under the terms of the MIT license. See the
[`LICENSE`](LICENSE) file for the copyright information and licensing terms.
