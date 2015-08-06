# `cth_readable`

An OTP library to be used for CT log outputs you want to be readable
around all that noise they contain.

It adds 3 levels of logging with colors:

Good results in green:

```
%%% mycode_SUITE ==> TestCase: OK
```

Errors in red:

```
%%% mycode_SUITE ==> TestCase (group GroupName): FAILED
%%% mycode_SUITE ==>    Stacktrace
```

Skipped cases in magenta:

```
%%% mycode_SUITE ==> TestCase (group GroupName): SKIPPED
%%% mycode_SUITE ==>    SkipReason
```

## Usage

To use it, add to your project as:

```
{ct_opts, [{ct_hooks, [cth_readable_shell]}]}.

{profiles, [
    {test,
    [{deps, [
        {cth_readable,
        {git, "https://github.com/ferd/cth_readable.git", {branch, "master"}}}
    ]}]}
]}.
```
