# erlang-standard-snippets

Sublime Text snippets (templates) for erlang standard library functions. This project exists to provide a stable form of auto completion for known erlang modules using the snippet system, instead of auto completion.

### How it works

The `esnipper` module is able to generate Sublime Text snippet files for functions exported from any compiled erlang module on the VMs path.

The snippet files are static and are not updated when functions are modified without manual intervention. This makes it useful for the standard library, or even third party dependencies but not usable to template application code that is being worked on.

### Development

Running esnipper from the shell gives nicely printed errors, to run in the shell execute the following:

```erlang
{ok, _} = c(esnipper, [debug_info]), esnipper:main([dict, lists]).
```