%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2016 8:17 PM
%%%-------------------------------------------------------------------
-module(pg_model_t_model).
-compile({parse_trans, exprecs}).
-author("simon").

%% API
%% callbacks
-export([
  pr_formatter/1
]).

%%-compile(export_all).
%%-------------------------------------------------------------
-define(TXN, mchants).

-type ts() :: erlang:timestamp().

-type id() :: non_neg_integer().
-type name() :: binary().
-type status() :: normal | forizon | closed.
-type payment_method() :: [gw_netbank | gw_wap | gw_app].


-export_type([id/0, name/0, status/0]).

-record(?TXN, {
  id = 0 :: id()
  , mcht_full_name = <<"">> :: name()
  , mcht_short_name = <<"">> :: name()
  , status = normal :: status()
  , payment_method = [gw_netbank] :: payment_method()
  , up_mcht_id = <<"">> :: binary()
  , quota = [{txn, -1}, {daily, -1}, {monthly, -1}] :: list()
  , up_term_no = <<"12345678">> :: binary()
  , update_ts = erlang:timestamp() :: ts()
  , field
}).
-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).

-export_records([?TXN]).
%%-------------------------------------------------------------
pr_formatter(Field)
  when (Field =:= mcht_full_name)
  or (Field =:= mcht_short_name)
  ->
  string;
pr_formatter(_) ->
  ok.
