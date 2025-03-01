[![Build Status](https://app.travis-ci.com/silviucpp/erluap.svg?branch=master)](https://travis-ci.com/github/silviucpp/erluap)
[![GitHub](https://img.shields.io/github/license/silviucpp/erluap)](https://github.com/silviucpp/erluap/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/erluap)](https://hex.pm/packages/erluap)

# erluap - Erlang User-Agent Parser

This is the Erlang implementation of [ua-parser][1]. It provides a NIF wrapper around [uap-cpp][2] for efficient user-agent parsing.

## Getting Started

### Dependencies

Ensure the following dependencies are installed before compiling:

- `re2`
- `yaml-cpp` (0.5 API)

##### MacOS Installation

You can install the required dependencies using Homebrew:

```bash
brew install re2 yaml-cpp
```

##### Ubuntu Installation

On Ubuntu, install the dependencies using APT:

```bash
sudo apt-get install libyaml-cpp-dev libre2-dev
```

### Integration

To add it as a dependency in your project, include the following in your `rebar.config`:

```erlang
{deps, [
  {erluap, ".*", {git, "https://github.com/silviucpp/erluap.git", "master"}}
]}.
```

### Updating User-Agent Regexes

The user-agent regex definitions are sourced from [uap-core][3]. To update them, modify `UAP_CORE_REV` inside `build_deps.sh` and rebuild.

---

## API

### Parsing as records

erluap returns parsed data as records. The structures are defined in `erluap.hrl`:

```erlang
-record(device, {
    device_type   :: device_type(),
    family :: value(),
    model :: value(),
    brand :: value()
}).

-record(agent, {
    family :: value(),
    version_major :: value(),
    version_minor :: value(),
    version_patch :: value(),
    version_patch_minor :: value()
}).
```

The `parse/1` function returns a tuple `{Device::#device{}, Os::#agent{}, Browser::#agent{}}`:

```erlang
erluap:parse(<<"Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148">>).

{{device,mobile,<<"iPhone">>,<<"iPhone">>,<<"Apple">>},
 {agent,<<"iOS">>,<<"12">>,<<"2">>,null,null},
 {agent,<<"Mobile Safari UI/WKWebView">>,null,null,null,null}}
```

### Parsing as proplist

erluap can also return parsed data as a proplist using `erluap:parse_as_proplist/1`:

```erlang
erluap:parse_as_proplist(<<"Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148">>).

[{device,[{device_type,mobile},
          {family,<<"iPhone">>},
          {model,<<"iPhone">>},
          {brand,<<"Apple">>}]},
 {os,[{family,<<"iOS">>},
      {version_major,<<"12">>},
      {version_minor,<<"2">>},
      {version_patch,null},
      {version_patch_minor,null}]},
 {browser,[{family,<<"Mobile Safari UI/WKWebView">>},
           {version_major,null},
           {version_minor,null},
           {version_patch,null},
           {version_patch_minor,null}]}]
```

### Detecting Crawlers (Spiders)

To check if a user-agent is a web crawler (spider), verify if the device family is `<<"Spider">>` or use the helper function:

```erlang
erluap:is_spider(UserAgent).
```

---

## Running Tests

To execute the test suite run:

```bash
make ct
```

---

[1]: https://github.com/ua-parser
[2]: https://github.com/ua-parser/uap-cpp
[3]: https://github.com/ua-parser/uap-core
