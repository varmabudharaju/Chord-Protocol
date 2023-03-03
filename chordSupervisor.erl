-module(chordSupervisor).

-behaviour(gen_server).

-export([start_link/2, handle_call/3, init/1, handle_cast/2, getNumberOfBitsForTable/1, getKeysList/3, initiateChordProtocol/2, performNetworkJoin/4, performRouting/5]).

-define(SERVER, ?MODULE).

start_link(NumberOfNodes, NumberOfRequests) ->
  gen_server:start_link(chordSupervisor, [NumberOfNodes, NumberOfRequests], []).

init([NumberOfNodes, NumberOfRequests]) ->
  {ok, {NumberOfNodes, NumberOfRequests,[]}}.

handle_cast({informSupervisor, AverageHopCountOfNode, PidOfSupervisor}, State) ->
  {NumberOfNodes, NumberOfRequests, HopList} = State,
  NewHopList = HopList ++ [AverageHopCountOfNode],
  if
    length(NewHopList) == NumberOfNodes ->
      {_, RunTimeTakenSinceLastCall} = statistics(wall_clock),
      io:fwrite("Average Hops taken are ~p in ~p milliseconds ~n",
        [(lists:sum(NewHopList) / NumberOfNodes), RunTimeTakenSinceLastCall]),
      exit(PidOfSupervisor, normal);
    true ->
      ok
  end,
  {noreply, {NumberOfNodes, NumberOfRequests, NewHopList}}.

handle_call(_, _, _) ->
  {noreply, {}}.

getNumberOfBitsForTable(NumberOfNodes) ->
  trunc(ceil(math:log2(NumberOfNodes))).

initiateChordProtocol(NumberOfNodes, NumberOfRequests) ->
  SizeOfTable = getNumberOfBitsForTable(NumberOfNodes),
  Iterator = trunc(math:pow(2, SizeOfTable)),
  KeysList = getKeysList(Iterator, NumberOfNodes, []),
  {_, PidOfSupervisor} = start_link(NumberOfNodes, NumberOfRequests),
  statistics(wall_clock),
  NodeList = performNetworkJoin(NumberOfNodes, KeysList, SizeOfTable, NumberOfRequests),
  performRouting(NumberOfNodes, KeysList, NumberOfRequests, NodeList, PidOfSupervisor).


getKeysList(Iterator, NodeLimit, KeyList) when (NodeLimit > 1) and (length(KeyList) == 0)->
  getKeysList(Iterator, NodeLimit, KeyList ++ [rand:uniform(Iterator)]);

getKeysList(Iterator, NodeLimit, KeyList) when NodeLimit > 1 ->
  Low = 1, High = Iterator - 1,
  KeyToAdd = rand:uniform(High - Low + 1) + Low - 1,

  case lists:member(KeyToAdd, KeyList) of
   true ->
     getKeysList(Iterator, NodeLimit, KeyList);
    false ->
      getKeysList(Iterator, NodeLimit - 1, KeyList ++ [KeyToAdd])
  end;

getKeysList(_, NodeLimit, KeyList) when NodeLimit =:= 1 ->
  lists:sort(KeyList).


performNetworkJoin(NumberOfNodes, KeysList, SizeOfTable, NumberOfRequests) ->
  NodeList = lists:foldl(
    fun(Index, Accumulator) ->
      {_, Pid} = chordNode:start_link(lists:nth(Index, KeysList), KeysList, SizeOfTable, NumberOfRequests),
      Accumulator ++ [Pid]
    end,
    [],
    lists:seq(1, NumberOfNodes)
  ),
  NodeList.

performRouting(NumberOfNodes, KeysList, NumberOfRequests, NodeList, PidOfSupervisor) ->
  lists:foldl(
    fun(Index, _) ->
      Pid = lists:nth(Index, NodeList),
      gen_server:cast(Pid, {initiate, NumberOfRequests, KeysList, NodeList, PidOfSupervisor, NumberOfNodes})
    end,
    [],
    lists:seq(1, NumberOfNodes)
  ).