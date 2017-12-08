-module(flow).

-export([make_flow/2, generate_flows/2]).

%% @doc Create a flow out of its steps.
make_flow(Name, Steps) ->
  [ {Event, Name} || Event <- Steps ]. 

%% @doc Generate all possible flows from two source flows.
generate_flows(A, B) ->
  generate_flows([], A, B, []).

generate_flows(Head, [], B, Flows) -> [Head ++ B|Flows];
generate_flows(Head, A, [], Flows) -> [Head ++ A|Flows];
generate_flows(Head, [HeadA|RestA]=A, [HeadB|RestB]=B, Flows) ->
  FlowsA = generate_flows(Head ++ [HeadA], RestA, B, Flows),
  generate_flows(Head ++ [HeadB], A, RestB, FlowsA).
 
