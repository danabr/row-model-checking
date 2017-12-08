-module(engine).

-export([execute/2]).

-record(vm_state, {
          step = 0,
          txns = []
        }).

%% @doc Execute a flow. Returns the final database state and VM state.
execute(Flow, InitialStates) ->
  execute(Flow, InitialStates, #vm_state{}).

execute([], States, #vm_state{txns=Txns}) ->
  DBState = dict:fetch(db, States),
  {DBState, lists:reverse(Txns)};
execute([{{write, Updates}, Name}|Rest], States, VMState) ->
  LocalVClock = proplists:get_value(vclock, dict:fetch(Name, States)),
  DBVClock = proplists:get_value(vclock, dict:fetch(db, States)),
  case LocalVClock of
    DBVClock  -> execute(Rest, write_single(Updates, Name, States), VMState);
    _Mismatch -> execute(Rest, write_siblings(Updates, Name, States), VMState)
  end;
execute([{get_resolve, Name}|Rest], States, VMState)              ->
  DBState = dict:fetch(db, States),
  case proplists:get_value(db_state, DBState) of
    undefined -> execute(Rest, get(Name, States), VMState);
    deleted   -> execute(delete_from_flow(Rest, Name), States, VMState); % Flow stopped (failed to read)
    siblings  -> execute(Rest, resolve_siblings_get(Name, States), VMState)
  end;
execute([{get_vclock_no_siblings, Name}|Rest], States, VMState)   ->
  DBState = dict:fetch(db, States),
  case proplists:get_value(db_state, DBState) of
    undefined -> execute(Rest, vclock_update(Name, States), VMState);
    deleted   -> execute(delete_from_flow(Rest, Name), States, VMState); % Flow stopped (failed to read)
    siblings  -> execute(delete_from_flow(Rest, Name), States, VMState)  % Flow stopped (siblings)
  end;
execute([{get_vclock, Name}|Rest], States, VMState)   ->
  DBState = dict:fetch(db, States),
  case proplists:get_value(db_state, DBState) of
    deleted -> execute(delete_from_flow(Rest, Name), States, VMState); % Flow stopped (failed to read)
    _ -> execute(Rest, vclock_update(Name, States), VMState)
  end;
% Not truly modeled after reality. We should do a get_vclock first,
% and add a sibling here if necessary.
execute([{delete, Name}|Rest], States, VMState)           ->
  LocalVClock = proplists:get_value(vclock, dict:fetch(Name, States)),
  DBVClock = proplists:get_value(vclock, dict:fetch(db, States)),
  case LocalVClock of
    DBVClock  -> execute(Rest, delete_single(Name, States), VMState);
    _Mismatch -> execute(Rest, delete_siblings(Name, States), VMState)
  end;
execute([{{check, Check}, Name}|Rest], States, VMState)   ->
  case check(Check, Name, States) of
    ok   -> execute(Rest, States, VMState);
    stop -> execute(delete_from_flow(Rest, Name), States, VMState) % Flow stopped (check failed)
  end;
execute([{{txn, report_state}, Name}|Rest], States, #vm_state{txns = Txns}=VMState) ->
  State = proplists:get_value(state, dict:fetch(Name, States)),
  execute(Rest, States, VMState#vm_state{txns=[{report, State}|Txns]});
execute([{{txn, Txn}, _}|Rest], States, #vm_state{txns = Txns}=VMState) ->
  execute(Rest, States, VMState#vm_state{txns=[Txn|Txns]}).

check(no_state, Name, States) ->
  case proplists:get_value(state, dict:fetch(Name, States)) of
    "" -> ok;
    _  -> stop
  end;
check(has_state, Name, States) ->
  case proplists:get_value(state, dict:fetch(Name, States)) of
    "" -> stop;
    _  -> ok
  end.

write_single(Updates, Name, States) ->
  LocalState = update_local_state(Updates, dict:fetch(Name, States)),
  DBVClock = proplists:get_value(vclock, dict:fetch(db, States)),
  NewDBState = [{vclock, [Name|DBVClock]}|LocalState],
  StatesAfterDBUpdate = dict:store(db, NewDBState, States),
  dict:store(Name, LocalState, StatesAfterDBUpdate).

write_siblings(Updates, Name, States) ->
  LocalState = update_local_state(Updates, dict:fetch(Name, States)),
  DBState = dict:fetch(db, States),
  DBVClock = proplists:get_value(vclock, DBState),

  NewDBState = [ {vclock, [Name|DBVClock]}
               , {db_state, siblings}
               , {siblings, [strip_vclock(LocalState)|siblings(DBState)]}
               ],
  StatesAfterDBUpdate = dict:store(db, NewDBState, States),
  dict:store(Name, LocalState, StatesAfterDBUpdate).

get(Name, States) ->
  DBState = dict:fetch(db, States),
  dict:store(Name, strip_db_state(DBState), States).

resolve_siblings_get(Name, States) ->
  VClock = [Name|proplists:get_value(vclock, dict:fetch(db, States))],
  dict:store(Name, [{state, pending}, {vclock, VClock}], States).

delete_single(Name, States) ->
  VClock = [Name|proplists:get_value(vclock, dict:fetch(db, States))],
  dict:store(db, [{db_state, deleted}, {vclock, VClock}], States).

delete_siblings(Name, States) ->
  DBState = dict:fetch(db, States),
  DBVClock = proplists:get_value(vclock, DBState),

  NewDBState = [ {vclock, [Name|DBVClock]}
               , {db_state, siblings}
               , {siblings, [[]|siblings(DBState)]}
               ],
  dict:store(db, NewDBState, States).

vclock_update(Name, States) ->
  DBVClock = proplists:get_value(vclock, dict:fetch(db, States)),
  NewState = [{vclock, DBVClock}|strip_vclock(dict:fetch(Name, States))],
  dict:store(Name, NewState, States).
  
update_local_state(Updates, LocalState) ->
  lists:ukeymerge(1, lists:keysort(1, Updates), LocalState).

strip_db_state(State) ->
  proplists:delete(db_state, State).

strip_vclock(State) -> 
  proplists:delete(vclock, State).

siblings(DBState) ->
  case proplists:get_value(siblings, DBState, []) of 
    []       -> [strip_vclock(strip_db_state(DBState))];
    Siblings -> Siblings
  end.

delete_from_flow(Flow, Name) ->
  lists:filter(fun({_, N}) -> N =/= Name end, Flow).

