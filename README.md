This is the Erlang implementation of [ua-parser][1]. The implementation is a NIF wrapper around [uap-cpp][2]. 

### Getting started:

##### Dependencies

In order to be able to compile properly make sure the following deps are available on your machine:

- boost_regex
- yaml-cpp (0.5 API)

On Mac Os you can install those using `brew` :

```bash
brew install boost
brew install yaml-cpp
``` 

On Ubuntu:

```bash
sudo apt-get install libyaml-cpp-dev
sudo apt-get install libboost-regex-dev
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
erluap:parse(<<"Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0">>).
{{device,<<"Other">>,null,null},
 {agent,<<"Windows 7">>,null,null,null,null},
 {agent,<<"Firefox">>,<<"47">>,<<"0">>,null,null}}
```

##### Parse as proplist

There is possible to parse the result as proplist using `erluap:parse_as_proplist/1` :

```erlang
erluap:parse_as_proplist(<<"Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0">>).
[{device,[{family,<<"Other">>},{model,null},{brand,null}]},
 {os,[{family,<<"Windows 7">>},
      {version_major,null},
      {version_minor,null},
      {version_patch,null},
      {version_patch_minor,null}]},
 {browser,[{family,<<"Firefox">>},
           {version_major,<<"47">>},
           {version_minor,<<"0">>},
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

