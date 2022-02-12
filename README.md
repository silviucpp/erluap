[![Build Status](https://travis-ci.com/silviucpp/erluap.svg?branch=master)](https://travis-ci.com/github/silviucpp/erluap)
[![GitHub](https://img.shields.io/github/license/silviucpp/erluap)](https://github.com/silviucpp/erluap/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/erluap)](https://hex.pm/packages/erluap)

This is the Erlang implementation of [ua-parser][1]. The implementation is a NIF wrapper around [uap-cpp][2]. 

### Getting started:

##### Dependencies

In order to be able to compile properly make sure the following deps are available on your machine:

- re2
- yaml-cpp (0.5 API)

On Mac Os you can install those using `brew` :

```bash
brew install re2 yaml-cpp
``` 

On Ubuntu:

```bash
sudo apt-get install libyaml-cpp-dev libre2-dev
```

##### Integration

The application is compatible with both `rebar` or `rebar3`. Add `erluap` as a rebar dependency to your project:

```
{deps, [
  {erluap, ".*", {git, "https://github.com/silviucpp/erluap.git", "master"}},
}.
```

##### Update user agents regexes

The file using user agents regular expressions is part of [uap-core][3]. In order to update this file you can modify
`UAP_CORE_REV` inside `build_deps.sh`

### API

##### Parse as records

Records are described in `erluap.hrl` as follow:

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

The parsing method returns a tuple as follow `{Device::#device{}, Os::#agent{}, Browser::#agent{}}` :

```erlang
erluap:parse(<<"Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148">>).

{{device,mobile,<<"iPhone">>,<<"iPhone">>,<<"Apple">>},
 {agent,<<"iOS">>,<<"12">>,<<"2">>,null,null},
 {agent,<<"Mobile Safari UI/WKWebView">>,null,null,null,null}}
```

##### Parse as proplist

There is possible to parse the result as proplist using `erluap:parse_as_proplist/1` :

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

###### Check if a device is a crowler

In order to check if a device is a crowler (spider) you cam check is device family is equal with `<<"Spider">>` or just use
the method `erluap:is_spider/1`

### Tests

In order to run the tests just use `make ct` from project root after you compiled and got the deps using `rebar`

[1]:https://github.com/ua-parser
[2]:https://github.com/ua-parser/uap-cpp
[3]:https://github.com/ua-parser/uap-core
