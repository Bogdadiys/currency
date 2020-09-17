PROJECT = currency
PROJECT_DESCRIPTION = test work
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = inets ssl
DEPS = cowboy jsx exomler

dep_cowboy  = git https://github.com/ninenines/cowboy.git   2.8.0
dep_jsx     = git git@github.com:talentdeficit/jsx.git     v3.0.0
dep_exomler = git git@github.com:erlangbureau/exomler.git  master

include erlang.mk
