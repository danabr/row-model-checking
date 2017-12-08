-module(validator).

-export([validate_flows/3, summarize/1]).

%% @doc Runs through a list of flows, with each flow starting in its given
%% state, and returns the end result of each flow, as classified by the given
%% end state verification function.
validate_flows([], _InitialStates, _)          -> [];
validate_flows([Flow|Flows], InitialStates, Verify) ->
  EndState = engine:execute(Flow, InitialStates),
  Result = case Verify(EndState) of
    true  -> {ok, EndState};
    false -> {error, EndState}
  end,
  [{Flow, Result}|validate_flows(Flows, InitialStates, Verify)].

%% @doc Summarizes flow results (as returned by validate_flows). Returns a count
%% of each end result (database state, and number of steps executed), and one
%% example per error flow.
summarize(FlowResults) ->
  { count_by_type(FlowResults)
  , example_of_each_error_state(FlowResults)
  }.

count_by_type(FlowResults) ->
  count_by_types(FlowResults, dict:new()).

count_by_types([], Stats) -> dict:to_list(Stats);
count_by_types([{_,  Result}|Results], Stats) ->
  count_by_types(Results, dict:update_counter(Result, 1, Stats)).

example_of_each_error_state(FlowResults) ->
  example_of_each_error_state(FlowResults, dict:new()).

example_of_each_error_state([], Stats) -> dict:to_list(Stats);
example_of_each_error_state([{_,  {ok, _}}|Results], Stats) ->
  example_of_each_error_state(Results, Stats);
example_of_each_error_state([{F,  Result}|Results], Stats) ->
  example_of_each_error_state(Results, dict:store(Result, F, Stats)).
