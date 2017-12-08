-module(examples).

-compile([export_all]).

test_simultaneous_restore_aborted_and_debit_flow() ->
  StartStates = [ {restore_aborted, []}
                , {debit, []}
                , {db, [{state, ""}, {vclock, []}]}
                ],
  validator:summarize(
    validator:validate_flows(
      flow:generate_flows(restore_aborted_flow(), debit_flow(debit))
    , dict:from_list(StartStates)
    , fun acceptable_restore_aborted_or_debit_end_state/1)).

test_simultaneous_debit_flow() ->
  StartStates = [ {debit1, []}
                , {debit2, []}
                , {db, [{state, ""}, {vclock, []}]}
                ],
  validator:summarize(
    validator:validate_flows(
      flow:generate_flows(debit_flow(debit1), debit_flow(debit2))
    , dict:from_list(StartStates)
    , fun acceptable_debit_end_state/1)).

test_simultaneous_restore_started_and_debit_flow() ->
  StartStates = [ {restore_started, []}
                , {debit, []}
                , {db, [{state, ""}, {vclock, []}]}
                ],
  validator:summarize(
    validator:validate_flows(
      flow:generate_flows(restore_started_flow(), debit_flow(debit))
    , dict:from_list(StartStates)
    , fun acceptable_restore_started_or_debit_end_state/1)).

restore_aborted_flow() ->
  flow:make_flow(restore_aborted, [ get_resolve
                                  , {check, no_state}
                                  , {write, [{state, aborted}]}, get_vclock_no_siblings % Missing today
                                  , {txn, report_state}
                                  , get_vclock, delete
                                  ]).

restore_started_flow() ->
  flow:make_flow(restore_started, [ get
                                  , {check, has_state}
                                  , {txn, report_state}
                                  , delete
                                  ]).

debit_flow(Name) ->
  flow:make_flow(Name, [ get_resolve
                       , {check, no_state}
                       , {write, [{state, pending}]}, get_vclock_no_siblings
                       , {txn, debit}
                       , {write, [{state, success}]}, get_vclock_no_siblings
                       , {write, []}, get_vclock_no_siblings
                       , {txn, report_state}
                       ]).

acceptable_restore_aborted_or_debit_end_state({DBState, Txns}) ->
  successful_debit_flow(DBState, Txns) orelse
  successful_restore_flow(DBState, Txns, aborted).

acceptable_restore_started_or_debit_end_state({DBState, Txns}) ->
  successful_debit_flow(DBState, Txns) orelse
  successful_restore_flow(DBState, Txns, success).

acceptable_debit_end_state({DBState, Txns}) ->
  successful_debit_flow(DBState, Txns).

successful_debit_flow(DBState, Txns) ->
  [debit, {report, success}] =:= Txns andalso
  proplists:get_value(db_state, DBState) =/= deleted.

successful_restore_flow(DBState, Txns, ReportState) ->
  [{report, ReportState}] =:= Txns andalso
  proplists:get_value(db_state, DBState) =:= deleted.
