# `cth_readable`

An OTP library to be used for CT log outputs you want to be readable
around all that noise they contain.

## What it looks like

![example](http://i.imgur.com/Bib9Ls0.png)

## Usage

To use it, add to your project as:

```erlang
{ct_opts, [{ct_hooks, [cth_readable_shell]}]}.

{profiles, [
    {test,
    [{deps, [
        {cth_readable,
        {git, "https://github.com/ferd/cth_readable.git", {branch, "master"}}}
    ]}]}
]}.
```

