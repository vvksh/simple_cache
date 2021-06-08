# Simple_cache

This is a caching application which can store key,value and let you retrieve value 
by key.

# Implementation
Implemented in erlang as a learning exercise for `Erlang and OTP in Action` book.


# Compile and Run
- Compile
```shell script
erlc src/*.erl
```

- Run
```shell script
viveks:simple_cache/ (master) $ erl -pa ebin                                                                                       [0:53:11]
Erlang/OTP 23 [erts-11.1.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

Eshell V11.1.3  (abort with ^G)
1> application:start(simple_cache).
ok
2> simple_cache:insert("test", 2).
true
3> simple_cache:lookup("test").
{ok,2}
4> simple_cache:delete("test").
ok
5> simple_cache:lookup("test").
{error,not_found}
```