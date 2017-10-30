-module(pg_model).
-include_lib("eunit/include/eunit.hrl").

-type pr_format() :: default | string.

%% callbacks
-callback pr_formatter(atom()) -> pr_format().
-optional_callbacks([pr_formatter/1]).

%% API exports
-export([
  name/1
  , fields/1
  , new/2
  , new_empty/1

  , get/3
  , get/4

  , get_proplist/3

  , set/3
  , set/4

  , inc/3
  , inc/4

  , to/3

  , from/2
  , pr/2
  , lager/3

]).
%% types
-type pg_model() :: tuple().
-export_type([pg_model/0]).

%%-compile(export_all).

-define(TEST_MODEL_TBL, mchants).
-define(TEST_MODEL, pg_model_t_model).

%%====================================================================
%% API functions
%%====================================================================
%% exprecs interface
name(M) when is_atom(M) ->
  [TableName] = M: '#exported_records-'(),
  TableName.

name_test() ->
  ?assertEqual(?TEST_MODEL_TBL, name(?TEST_MODEL)),
  ok.
%%-----------------------------------------------------------------
fields(M) when is_atom(M) ->
  TableName = name(M),
  Fields = M: '#info-'(TableName, fields),
  Fields.

fields_test() ->
  ?assertEqual([id, mcht_full_name, mcht_short_name, status
    , payment_method, up_mcht_id, quota, up_term_no, update_ts
    , field]
    , fields(?TEST_MODEL)),
  ok.
%%-----------------------------------------------------------------
new_empty(M) when is_atom(M) ->
  TableName = name(M),
  M: '#new-'(TableName).

new_empty_test() ->
  A = new_empty(?TEST_MODEL),
  TS = get(?TEST_MODEL, A, update_ts),
  ?assertEqual({?TEST_MODEL_TBL, 0, <<"">>, <<"">>, normal, [gw_netbank], <<"">>,
    [{txn, -1}, {daily, -1}, {monthly, -1}], <<"12345678">>, TS,
    undefined}
    , A),
  ok.

%%-----------------------------------------------------------------
new(M, List) when is_atom(M), is_list(List) ->
  ListCleaned = clean_model_list(M, List),
  EmptyRec = new_empty(M),
  NewRec = M: '#set-'(ListCleaned, EmptyRec),
  NewRec;

new(M, Map) when is_atom(M), is_map(Map) ->
  List = maps:to_list(Map),
  new(M, List).

new_test() ->
  ?assertEqual(new(?TEST_MODEL, [{id, 1}, {mcht_full_name, <<"aaa">>}, {update_ts, <<>>}, {field, undefined}])
    , new(?TEST_MODEL, #{id=>1, mcht_full_name=><<"aaa">>, update_ts => <<>>})),
  ?assertEqual(new(?TEST_MODEL, [{id, 1}, {mcht_full_name, <<"aaa">>}, {update_ts, <<>>}, {field, undefined}, {aa, 1}])
    , new(?TEST_MODEL, #{id=>1, mcht_full_name=><<"aaa">>, update_ts => <<>>})),
  ok.


%%-------------------------------------------------------------------
clean_model_list(M, List) ->
  %% remove unexisted keys in M:fields() from List
  F =
    fun(Key, Acc) ->
      AccNew = proplists:delete(Key, Acc),
      AccNew
    end,
  UnexistedKeys = unexisted_keys(M, proplists:get_keys(List)),
  lists:foldl(F, List, UnexistedKeys).

clean_model_list_test() ->
  ?assertEqual([{id, 1}, {mcht_full_name, <<"aaa">>}, {update_ts, <<>>}, {field, undefined}],
    clean_model_list(?TEST_MODEL, [{id, 1}, {mcht_full_name, <<"aaa">>}, {update_ts, <<>>}, {field, undefined}, {aa, 1}])),
  ok.

%%-------------------------------------------------------------------
unexisted_keys(M, KeyList) ->
  Fields = fields(M),
  lists:subtract(KeyList, Fields).

unexisted_keys_test() ->
  ?assertEqual([aa], unexisted_keys(?TEST_MODEL, [id, mcht_full_name, update_ts, field, aa])),
  ok.
%%-------------------------------------------------------------------
%% getter/setter
get(M, Repo, Key, Default) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  case get(M, Repo, Key) of
    undefined -> Default;
    Value -> Value
  end.

get(M, Repo, Key) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
%%  Value = apply(M, '#get-', [Key, Repo]),
  Value = M: '#get-'(Key, Repo),
  Value;
get(M, Repo, Keys) when is_atom(M), is_tuple(Repo), is_list(Keys) ->
%%  Values = apply(M, '#get-', [Keys, Repo]),
%%  Values.
  [get(M, Repo, Key) || Key <- Keys].

get_test() ->
  R = t_new(model),
  ?assertEqual(1, get(?TEST_MODEL, R, id)),
  ?assertEqual(<<"full">>, get(?TEST_MODEL, R, mcht_full_name)),

  ?assertEqual([1, <<"full">>], get(?TEST_MODEL, R, [id, mcht_full_name])),

  ?assertEqual(undefined, get(?TEST_MODEL, R, field)),
  ?assertEqual(defined, get(?TEST_MODEL, R, field, defined)),
  ?assertEqual(1, get(?TEST_MODEL, R, id, 2)),

  ok.
%%-------------------------------------------------------------------
-spec get_proplist(M, Model, Keys) -> Result when
  M :: atom(),
  Model :: tuple(),
  Keys :: [atom()],
  Result :: proplists:proplist().

get_proplist(M, Model, Keys) when is_atom(M), is_tuple(Model), is_list(Keys) ->
  [{Key, get(M, Model, Key)} || Key <- Keys].

get_proplist_test() ->
  R = t_new(model),
  ?assertEqual([{id, 1}, {mcht_full_name, <<"full">>}], get_proplist(?TEST_MODEL, R, [id, mcht_full_name])),
  ok.

%%-------------------------------------------------------------------
set(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
set(M, Repo, Key, Value) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  ValueList = [{Key, Value}],
  set(M, Repo, ValueList).

set(M, Repo, ValueLists) when is_atom(M), is_tuple(Repo), is_list(ValueLists) ->
  %%TableName = name(M),
%%  RepoNew = apply(M, '#set-', [ValueLists, Repo]),
  RepoNew = M: '#set-'(ValueLists, Repo),
  RepoNew.

set_test() ->
  R = t_new(model),
  ?assertEqual({error, pk_could_not_be_changed}, set(?TEST_MODEL, R, id, 333)),
  R1 = set(?TEST_MODEL, R, mcht_full_name, <<"new full">>),
  ?assertEqual(<<"new full">>, get(?TEST_MODEL, R1, mcht_full_name)),

  R2 = set(?TEST_MODEL, R, [{mcht_full_name, <<"new full">>}, {update_ts, 200}]),
  ?assertEqual(<<"new full">>, get(?TEST_MODEL, R2, mcht_full_name)),
  ?assertEqual(200, get(?TEST_MODEL, R2, update_ts)),

  ok.
%%-------------------------------------------------------------------
inc(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
inc(M, Repo, Key, IncValue) when is_atom(M), is_tuple(Repo), is_atom(Key), is_integer(IncValue) ->
  OldValue = get(M, Repo, Key),
  ValueList = [{Key, OldValue + IncValue}],
%%  RepoNew = apply(M, '#set-', [ValueList, Repo]),
  RepoNew = M: '#set-'(ValueList, Repo),
  RepoNew.

inc(M, Repo, {Key, IncValue}) when is_atom(M), is_tuple(Repo), is_integer(IncValue), is_atom(Key) ->
  inc(M, Repo, Key, IncValue).

inc_test() ->
  R = t_new(model),
  ?assertEqual({error, pk_could_not_be_changed}, inc(?TEST_MODEL, R, id, 1)),
  ?assertEqual(set(?TEST_MODEL, R, update_ts, 101), inc(?TEST_MODEL, R, update_ts, 1)),
  ?assertEqual(set(?TEST_MODEL, R, update_ts, 102), inc(?TEST_MODEL, R, {update_ts, 2})),

  ok.


%%-------------------------------------------------------------------
to(M, Repo, proplists) when is_tuple(Repo) ->
  to_proplists(M, Repo);
to(M, Repo, {proplists, OutFields}) when is_tuple(Repo), is_list(OutFields) ->
  to_proplists(M, Repo, OutFields);
to(M, Repo, {proplists, OutFields, In2OutMap}) when is_tuple(Repo), is_list(OutFields) ->
  to_proplists(M, Repo, {OutFields, In2OutMap});
to(M, Repo, map) when is_tuple(Repo) ->
  to_map(M, Repo);
to(M, Repo, model) when is_tuple(Repo) ->
  to_map(M, Repo);
to(M, Repos, model) when is_list(Repos) ->
  to_model(M, Repos);
to(M, Repo, poststring) when is_tuple(Repo) ->
  to_post(M, Repo, string);
to(M, Repo, {poststring, OutFields}) when is_tuple(Repo), is_list(OutFields) ->
  to_post(M, Repo, {OutFields}, string);
to(M, Repo, {poststring, OutFields, In2OutFieldMap})
  when is_tuple(Repo), is_list(OutFields), is_map(In2OutFieldMap) ->
  to_post(M, Repo, {OutFields, In2OutFieldMap}, string).

to_test() ->
  R1 = t_new(model),
  ?assertEqual(
    [
      {id, 1}
      , {mcht_full_name, <<"full">>}
      , {mcht_short_name, <<"short">>}
      , {status, normal}
      , {payment_method, [gw_netbank]}
      , {up_mcht_id, <<>>}
      , {quota, [{txn, -1}, {daily, -1}, {monthly, -1}]}
      , {up_term_no, <<"12345678">>}
      , {update_ts, 100}
      , {field, undefined}
    ]
    , to(?TEST_MODEL, R1, proplists)),

  R2 = t_new(model),
  ?assertEqual(
    #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
      , status=>normal, payment_method =>[gw_netbank]
      , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
      , up_term_no=> <<"12345678">>, update_ts => 100
      , field => undefined
    }
    , to(?TEST_MODEL, R2, map)),

  R3 = t_new(model),
  R31 = set(?TEST_MODEL, R3, [{quota, <<>>}, {payment_method, <<>>}]),
  ?assertEqual(
    <<"id=1&mcht_full_name=full&mcht_short_name=short&status=normal&up_term_no=12345678&update_ts=100">>
    , list_to_binary(to(?TEST_MODEL, R31, poststring))
  ),

  R4 = t_new(model),
  R41 = inc(?TEST_MODEL, R4, update_ts, 1),
%%  L = to(?TEST_MODEL, R, proplists),

  ?assertEqual(
    #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
      , status=>normal, payment_method =>[gw_netbank]
      , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
      , up_term_no=> <<"12345678">>, update_ts => 100
      , field=> undefined
    }
    , to(?TEST_MODEL, R4, model)),
  ?assertEqual(
    [
      #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
        , status=>normal, payment_method =>[gw_netbank]
        , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
        , up_term_no=> <<"12345678">>, update_ts => 100
        , field=> undefined
      }
      ,
      #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
        , status=>normal, payment_method =>[gw_netbank]
        , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
        , up_term_no=> <<"12345678">>, update_ts => 101
        , field=> undefined
      }
    ], to(?TEST_MODEL, [R4, R41], model)),


  %% post outfiled
  ?assertEqual(<<"id=1&mcht_full_name=full">>,
    list_to_binary(to(?TEST_MODEL, R4, {poststring, [id, mcht_full_name]}))),
  ?assertEqual(<<"MchtId=1&MchtFullName=full">>,
    list_to_binary(to(?TEST_MODEL, R4,
      {poststring,
        [id, mcht_full_name],
        #{id=>{<<"MchtId">>, integer}, mcht_full_name=><<"MchtFullName">>}
      }
    ))),
  ?assertEqual([{<<"MchtId">>, <<"1">>}, {<<"MchtFullName">>, <<"full">>}],
    to(?TEST_MODEL, R4,
      {proplists,
        [id, mcht_full_name],
        #{id=>{<<"MchtId">>, integer}, mcht_full_name=><<"MchtFullName">>}
      }
    )),

  ok.

%%-------------------------------------------------------------------
%% model (map) <==> repo(record)
to_proplists(M, Repo) when is_atom(M), is_tuple(Repo) ->
  Fields = fields(M),
  ValueList = tl(tuple_to_list(Repo)),
%%  lager:info("Fields = ~p,ValueList = ~p", [Fields, ValueList]),
  Ret = lists:zip(Fields, ValueList),
%%  lager:info("Ret = ~p", [Ret]),
  Ret.

to_proplists(M, Repo, {OutFields}) when is_atom(M), is_tuple(Repo), is_list(OutFields) ->
  VL = [{Field, pg_model:get(M, Repo, Field)} || Field <- OutFields],
  VL;
to_proplists(M, Repo, {OutFields, In2OutMap})
  when is_atom(M), is_tuple(Repo), is_list(OutFields), is_map(In2OutMap) ->
  F = fun
        (Field) ->
          case maps:get(Field, In2OutMap) of
            {KeyName, _Type} ->
              %% value must be integer, convert it to binary
              KeyName;
            KeyName ->
              KeyName
          end
      end,
  FValue = fun
             (Value) when is_integer(Value) ->
               integer_to_binary(Value);
             (Value) ->
               Value
           end,

  VL = [{F(Field), FValue(pg_model:get(M, Repo, Field))} || Field <- OutFields],
  VL.

%%-------------------------------------------------------------------
to_map(M, Repo) when is_atom(M), is_tuple(Repo) ->
  List = to_proplists(M, Repo),
  maps:from_list(List).

%%-------------------------------------------------------------------
to_post(M, Repo, string) when is_atom(M), is_tuple(Repo) ->
  PL = to_proplists(M, Repo),
  xfutils:post_vals_to_iolist(PL).

to_post(M, Repo, OutOption, string) when is_atom(M), is_tuple(Repo), is_tuple(OutOption) ->
  PL = to_proplists(M, Repo, OutOption),
  xfutils:post_vals_to_iolist(PL).
%%-------------------------------------------------------------------

to_model(M, List) when is_atom(M), is_list(List) ->
  [to_model(M, Repo) || Repo <- List];
to_model(M, Repo) when is_atom(M), is_tuple(Repo) ->
  to_map(M, Repo).
%%-------------------------------------------------------------------
from(M, List) when is_atom(M), is_list(List) ->
  EmptyModel = new_empty(M),
  M:'#fromlist-'(List, EmptyModel);
from(M, Map) when is_atom(M), is_map(Map) ->
  List = maps:to_list(Map),
  from(M, List).


from_test() ->
  R = t_new(model),
  Model =
    #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
      , status=>normal, payment_method =>[gw_netbank]
      , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
      , up_term_no=> <<"12345678">>, update_ts => 100
    },
  List = maps:to_list(Model),
  ?assertEqual(R, from(?TEST_MODEL, Model)),
  ?assertEqual(R, from(?TEST_MODEL, List)),
  ok.
%%-------------------------------------------------------------------
pr(M, Repo) when is_atom(M), is_tuple(Repo) ->
  VL = to_proplists(M, Repo),
  L = [pr_field(M, Field, Value) || {Field, Value} <- VL],
  lists:flatten(L);
pr(M, Model) when is_atom(M), is_map(Model) ->
  L = [pr_field(M, Field, maps:get(Field, Model)) || Field <- maps:keys(Model)],
  lists:flatten(L).

pr_field(M, Field, Value) ->
  ValueFormatter = case M:pr_formatter(Field) of
                     ok ->
                       %% default
                       "~p=~p,";
                     default ->
                       %% default
                       "~p=~p,";
                     string ->
                       "~p=~ts,"
                   end,
  io_lib:format(ValueFormatter, [Field, Value]).

pr_test() ->
  R = t_new(model),

  Out = pr(?TEST_MODEL, R),
  OutTrim = trim_pretty(Out),
  Expected = <<"id=1,mcht_full_name=full,mcht_short_name=short,status=normal,payment_method=[gw_netbank],up_mcht_id=<<>>,quota=[{txn,-1},{daily,-1},{monthly,-1}],up_term_no=<<\"12345678\">>,update_ts=100,field=undefined,">>,
  [E] = io_lib:format("~ts", [Expected]),

  ?assertEqual(E, OutTrim),

  Expected1 = "field=undefined,id=1,mcht_full_name=full,mcht_short_name=short,payment_method=[gw_netbank],quota=[{txn,-1},{daily,-1},{monthly,-1}],status=normal,up_mcht_id=<<>>,up_term_no=<<\"12345678\">>,update_ts=100,",
  [E1] = io_lib:format("~ts", [Expected1]),
  ?assertEqual(E1, trim_pretty(pr(?TEST_MODEL, to(?TEST_MODEL, R, map)))),

  R1 = set(?TEST_MODEL, R, mcht_full_name, <<"测试"/utf8>>),
  Exp2 = <<"id=1,mcht_full_name=测试,mcht_short_name=short,status=normal,payment_method=[gw_netbank],up_mcht_id=<<>>,quota=[{txn,-1},{daily,-1},{monthly,-1}],up_term_no=<<\"12345678\">>,update_ts=100,field=undefined,"/utf8>>,
  [E2] = io_lib:format("~ts", [Exp2]),
  ?assertEqual(E2, trim_pretty(pr(?TEST_MODEL, R1))),

  R2 = t_new(repo),
  Out2 = pr(?TEST_MODEL, R2),
  OutTrim2 = trim_pretty(Out2),

  ?assertEqual(E, OutTrim2),
  ok.

trim_pretty(L) ->
  F = fun(Char, AccIn) when (Char =:= 32) or (Char =:= 10) ->
    AccIn;
    (Char, AccIn) ->
      [Char | AccIn]
      end,

  RL = lists:foldl(F, [], L),
  lists:reverse(RL).


%%-------------------------------------------------------------------
lager(Level, M, Model) ->
  String = pr(M, Model),
  lager_out(Level, M, String).

lager_out(debug, M, String) ->
  lager:debug("~p = ~ts", [M, String]);
lager_out(info, M, String) ->
  lager:info("~p = ~ts", [M, String]);
lager_out(error, M, String) ->
  lager:error("~p = ~ts", [M, String]).
%%-------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================
t_new(repo) ->
  new(?TEST_MODEL, #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>, update_ts=>100});
t_new(model) ->
  new(?TEST_MODEL, #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>, update_ts=>100}).


