# `cth_readable`

An OTP library to be used for CT log outputs you want to be readable
around all that noise they contain.

There are currently two hooks:

1. `cth_readable_shell`, which shows failure stacktraces in the shell and
   otherwise shows successes properly, in color.
2. `cth_readable_failonly`, which only outputs error and SASL logs to the
   shell in case of failures. It also provides `cthr:pal/1-4` functions,
   working like `ct:pal/1-4`, but being silenceable by that hook.
3. `cth_readable_nosasl`, which disables all SASL logging. It however requires
   to be run *before* `cth_readable_failonly` to work.

## What it looks like

![example](http://i.imgur.com/dDFNxZr.png)
![example](http://i.imgur.com/RXZBG7H.png)

## Usage

To use it, add to your project as:

```erlang
{ct_opts, [{ct_hooks, [cth_readable_failonly, cth_readable_shell]}]}.

{profiles, [
    {test,
    [{deps, [
        {cth_readable,
        {git, "https://github.com/ferd/cth_readable.git", {branch, "master"}}}
    ]}]}
]}.
```

