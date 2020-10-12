# CauDEr

A Causal-Consistent Reversible Debugger for Erlang

![CauDEr screenshot](screenshot.png)

## Requirements

- Erlang/OTP >= 23.1

## Building

You can compile CauDEr by running the command: `make`

## Running

CauDEr has two components the debugger itself and a GUI.
These two can be run independently of each other.

### Erlang shell

You can use the Erlang shell to start the debugger and GUI servers.

```
Eshell V11.1  (abort with ^G)
1> cauder:start(). % Starting the debugger
{ok,<0.80.0>}
2> cauder_wx:start(). % Starting the GUI
{ok,<0.82.0>,{wx_ref,35,wxFrame,<0.82.0>}}
```

However, starting the GUI server will also start the debugger if it is not running yet.
In that case you can get PID of the debugger by calling `whereis(cauder).`

```
Eshell V11.1  (abort with ^G)
1> cauder_wx:start(). % Starting the GUI and the debugger
{ok,<0.81.0>,{wx_ref,35,wxFrame,<0.81.0>}}
2> whereis(cauder). % Getting the debugger PID
<0.80.0>
```

### Shell script

Alternatively, you can execute the script `cauder.sh`, that was generated during compilation, to start CauDEr

## Usage

For more information about how to use CauDEr you can check the [Wiki](https://github.com/mistupv/cauder-v2/wiki) (Under construction!)

## License

See [LICENSE](LICENSE) file.
