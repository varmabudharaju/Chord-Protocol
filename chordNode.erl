
-module(chordNode).

-behaviour(gen_server).

-export([start_link/4]).
-export([init/1, handle_cast/2, handle_call/3]).

-define(SERVER, ?MODULE).

start_link(NodeId, KeysList, SizeOfTable, NumberOfRequests) ->
  gen_server:start_link(chordNode, [NodeId, KeysList, SizeOfTable, NumberOfRequests], []).

init([NodeId, KeysList, SizeOfTable, NumberOfRequests]) ->
  FingerTableForNode = lists:sort(populateFingerTable(NodeId, SizeOfTable, 1, KeysList, [])),
  {ok, {NumberOfRequests, [], NodeId, FingerTableForNode, 0, 0, [], 0}}.

handle_cast({initiate, NumberOfRequests, KeysList, NodeList, PidOfSupervisor, NumberOfNodes}, State) ->
  {NumberOfRequests, _Keys, NodeId, FingerTableForNode, Target, HopCount, HopList, SourceOfCall} = State,
  createLookupForNode(NodeId, 0, NumberOfRequests, KeysList, FingerTableForNode, 0, NodeList, PidOfSupervisor, NumberOfNodes),
  {noreply, {NumberOfRequests, KeysList, NodeId, FingerTableForNode, Target, HopCount, HopList, SourceOfCall}};


handle_cast({keyFound, HopCountReceived, _KeysList, _NodeList, _FingerTableOfCallingNode, _Key, PidOfSupervisor, NumberOfNodes}, State) ->
  {NumberOfRequests, _Keys, NodeId, _FingerTableForNode, _Target, _HopCount, HopList, _SourceOfCall} = State,
  Threshold = getThresholdOfCount(NumberOfNodes),
  NewHopList = if
    (HopCountReceived < Threshold) -> HopList ++ [HopCountReceived];
    true -> HopList ++ [lists:nth(1, HopList)]
  end,
  if
    length(NewHopList) == NumberOfRequests ->
%%      io:fwrite("NodeID ~p => HopList = ~p Average = ~p ~n", [NodeId, NewHopList, (lists:sum(NewHopList) / length(NewHopList))]),
      gen_server:cast(PidOfSupervisor, {informSupervisor, (lists:sum(NewHopList) / length(NewHopList)), PidOfSupervisor});
    true ->
      ok
  end,
  {noreply, {NumberOfRequests, _Keys, NodeId, _FingerTableForNode, _Target, HopCountReceived, NewHopList, _SourceOfCall}};

handle_cast({lookup, Key, SourceOfCall, HopCountReceived, KeysList, NodeList, PidOfSupervisor, NumberOfNodes}, State) ->
  {NumberOfRequests, _Keys, NodeId, FingerTableForNode, _Target, _HopCount, _HopList, _SourceOfCall} = State,
  if
    Key == NodeId ->
      SourcePid = getNodePid(SourceOfCall, KeysList, NodeList),
      UpdatedHopCountReceived = HopCountReceived + 1,
      gen_server:cast(SourcePid, {keyFound, UpdatedHopCountReceived, KeysList, NodeList, FingerTableForNode, Key, PidOfSupervisor, NumberOfNodes}),
      {noreply, {NumberOfRequests, _Keys, NodeId, FingerTableForNode, Key, UpdatedHopCountReceived, _HopList, _SourceOfCall}};
    true ->
      DestinationIndex = findDestinationIndex(Key, FingerTableForNode, 1),
      DestinationNode = lists:nth(DestinationIndex, FingerTableForNode),
      DestinationNodePid = getNodePid(DestinationNode, KeysList, NodeList),
      gen_server:cast(DestinationNodePid, {lookup, Key, SourceOfCall, HopCountReceived + 1, KeysList, NodeList, PidOfSupervisor, NumberOfNodes}),
      {noreply, {NumberOfRequests, _Keys, NodeId, FingerTableForNode, Key, HopCountReceived, _HopList, _SourceOfCall}}
  end.

handle_call(_Request, _From, {}) ->
  {noreply, {}}.

getThresholdOfCount(NumberOfNodes) ->
  NumberOfNodes * 0.2.

populateFingerTable(NodeId, SizeOfTable, Iterator, KeysList, FingerTableForNode) when Iterator =< SizeOfTable ->
  LowerBound = (NodeId + trunc(math:pow(2, Iterator - 1))) rem trunc(math:pow(2, SizeOfTable)),
  UpperBound = (NodeId + trunc(math:pow(2, Iterator))) rem trunc(math:pow(2, SizeOfTable)),

  NewFingerTable =
    if
      LowerBound < UpperBound ->
        Temp = lists:filter(fun(Value) -> Value >= LowerBound end, KeysList),
        if
           length(Temp) > 0 ->
            Temp;
          true ->
            UpdatedTemp = lists:filter(fun(Value) -> Value < LowerBound end, KeysList),
            lists:sort(UpdatedTemp)
        end;
      true ->
        UpdatedTemp = lists:filter(fun(Value) -> Value < LowerBound end, KeysList),
        NewlyMappedTemp = lists:map(fun(Value) -> Value + trunc(math:pow(2, SizeOfTable)) end, UpdatedTemp),
        lists:sort(NewlyMappedTemp ++ lists:filter(fun(Value) -> Value >= LowerBound end, KeysList))
  end,
  NewFingerTableToAdd =
    case lists:nth(1, NewFingerTable) >= math:pow(2, SizeOfTable) of
       true ->
         [trunc(lists:nth(1, NewFingerTable)) rem trunc(math:pow(2, SizeOfTable))];
      false ->
        [lists:nth(1, NewFingerTable)]
    end,
  populateFingerTable(
    NodeId, SizeOfTable, Iterator + 1, KeysList, FingerTableForNode ++ NewFingerTableToAdd
  );

populateFingerTable(_NodeId, SizeOfTable, Iterator, _KeysList, FingerTableForNode) when Iterator > SizeOfTable ->
  FingerTableForNode.

createLookupForNode(NodeId, Iterator, IterationLimit, KeysList, FingerTableForNode, HopCount, NodeList, PidOfSupervisor, NumberOfNodes) when Iterator < IterationLimit ->
  Key = getRandomKeyFromKeysList(KeysList, NodeId),
  DestinationIndex = findDestinationIndex(Key, FingerTableForNode, 1),
  DestinationNode = lists:nth(DestinationIndex, FingerTableForNode),
  DestinationNodePid = getNodePid(DestinationNode, KeysList, NodeList),
  gen_server:cast(DestinationNodePid, {lookup, Key, NodeId, HopCount, KeysList, NodeList, PidOfSupervisor, NumberOfNodes}),
  createLookupForNode(NodeId, Iterator + 1, IterationLimit, KeysList, FingerTableForNode, HopCount, NodeList, PidOfSupervisor, NumberOfNodes);

createLookupForNode(_NodeId, Iterator, IterationLimit, _KeysList, _FingerTableForNode, _HopCount, _NodeList, _PidOfSupervisor, _NumberOfNodes) when Iterator == IterationLimit ->
  ok.


getRandomKeyFromKeysList(KeysList, NodeId) ->
  RandomKeyToSend = lists:nth(rand:uniform(length(KeysList)), KeysList),
  case RandomKeyToSend == NodeId of
    true -> getRandomKeyFromKeysList(KeysList, NodeId);
    false -> RandomKeyToSend
  end.

findDestinationIndex(Target, FingerTable, Iterator) when Iterator < length(FingerTable) ->
  case lists:nth(Iterator, FingerTable) == Target of
    true -> Iterator;
    false ->
      case lists:nth(Iterator, FingerTable) > Target of
        true ->
          if
            Iterator == 1 -> Iterator;
            true -> Iterator - 1
          end;
        false -> findDestinationIndex(Target, FingerTable, Iterator + 1)
      end
  end;

findDestinationIndex(Target, FingerTable, Iterator) when Iterator == length(FingerTable) ->
  case lists:nth(Iterator, FingerTable) == Target of
    true -> Iterator;
    false ->
      case lists:nth(Iterator, FingerTable) > Target of
        true -> Iterator - 1;
        false -> Iterator
      end
  end.

getNodePid(Node, KeysList, NodeList) ->
  lists:nth(string:str(KeysList, [Node]), NodeList).
