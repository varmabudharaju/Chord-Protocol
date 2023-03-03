# DOSP Project 3: Chord - P2P System and Simulation
****
## Group Details
<p><strong>Name: [ Pradyumna_Pasumarty : Sai Ram Varma_Budharaju ]</strong></p>
<strong>Members</strong>
<ol>
    <li>Sai Ram Varma Budharaju (UFID: 3337-4276 | <a>sbudharaju@ufl.edu</a>)</li>
    <li>Pradyumna Pasumarty (UFID: 6907-9707 | <a>ppasumarty@ufl.edu</a>)</li>
</ol>

****

## Project Description

The main goal of this project is to implement the Chord Protocol for object access services using Erlang's Actor Model.
This project determines the Average Hops required in a Chord Ring of different number of nodes and requests.

****

## Design Details

<ul>
    <li>
        Network Join
        <ul>
            <li>A Chord Supervisor is first created. Then, it initiates the Join activity and later triggers the routing</li> 
            <li>Before joining nodes to form a ring, the Supervisor computes the value of 'm' (i.e. SizeOfTables)</li>
            <li>It also computes the Keys that would be utilized throughout the program to emulate a Distributed Hash Table</li>
            <li>In order to keep the implementation simple, the Keys are just random indices in the range of 1 to N (NumberOfNodes)</li>
            <li>Once this metadata is prepared, the Supervisor iteratively creates Chord Child Actors whose Ids are in the above range.</li> 
            <li>The responsibility of creating FingerTables based on assigned IDs is delegated to the Chord Child Actors themselves</li>
            <li>The Process Ids (PIDs) of these children are maintained for ease of access</li>
        </ul>
    </li>
    <br/>
    <li>
        Routing
        <ul>
            <li>On successful joining of the nodes, the Supervisor initiates the Routing procedure on the Child Actors</li>
            <li>Each Child picks a random Key from the global Keys. It is ensured that this key, is different from the Node's own ID</li>
            <li>The Child now starts performing a lookup for this Key on its FingerTable and picks a suitable Destination Peer</li>
            <li>If the Destination Peer holds the required Key, HopCount & HopList are updated accordingly</li>
            <li>If the Destination Peer fails to find the key, it increments the HopCount & initiates a lookup on its FingerTable</li>
            <li>As and when a Node reaches the limit of its NumberOfRequests, it informs the Supervisor of its average Hop Count</li>
            <li>Once all the nodes have stabilized i.e. reached their limit of Requests, the Supervisor terminates the program</li>
            <li>The Supervisor duly outputs the Average Hops required by the nodes for the given input parameters</li>
        </ul>
    </li>
</ul>
<br/>

****
##  What is working ?
<br/>
This project successfully implemented Network Join and Routing for various permutations of Number of Nodes & Requests.
<br/>
<ul>
    <b>Execution Steps</b>
    <li>Move to the directory with the Chord Project</li>
    <li>Compile both ChordSupervisor and ChordNode using the commands <code>c(ChordSupervisor)</code> & <code>c(ChordNode)</code>.</li>
    <li>Initiate Chord Protocol using the command <code>initiateChordProtocol(NumberOfNodes, NumberOfRequests)</code></li>
    <li>The parameters NumberOfNodes & NumberOfRequests should be provided as input</li>
</ul>

## What is the largest network you managed to deal with?

The largest network we managed to deal with is as follows.
<ol>
    <li>Number of Nodes: 1400</li>
    <li>Number of Requests: 100</li>
    <li>Average Hops Required: 14.79 ~ log2(1400)</li>
</ol>

### Execution Results
The below data has been tabulated based on cases wherein the Number of Nodes and Number of Requests yielded an Average Hop Count that was closest to the value of log2(N) (where N = Number of Nodes) 
<br/>
<table>
    <th>Number of Nodes</th>
    <th>Number of Requests</th>
    <th>Average Hop Count</th>
    <tr>
        <td>100</td>
        <td>10</td>
        <td>3.654</td>
    </tr>
    <tr>
        <td>150</td>
        <td>40</td>
        <td>4.229</td>
    </tr>
    <tr>
        <td>200</td>
        <td>60</td>
        <td>4.743</td>
    </tr>
    <tr>
        <td>250</td>
        <td>30</td>
        <td>5.255</td>
    </tr>
    <tr>
        <td>300</td>
        <td>20</td>
        <td>5.818</td>
    </tr>
    <tr>
        <td>400</td>
        <td>15</td>
        <td>6.753</td>
    </tr>
    <tr>
        <td>500</td>
        <td>35</td>
        <td>7.643</td>
    </tr>
    <tr>
        <td>700</td>
        <td>55</td>
        <td>9.088</td>
    </tr>
    <tr>
        <td>800</td>
        <td>20</td>
        <td>10.255</td>
    </tr>
    <tr>
        <td>1000</td>
        <td>100</td>
        <td>11.739</td>
    </tr>
</table>

****

## Observations
<ol>
    <li>For most of the executions, the Average Hop Count obtained is always close to log2(NumberOfNodes) as indicated in the research paper. But a key observation made was that for higher numbers of nodes, the deviation is more.</li>
    <li>Since m = log2(N) is used to obtain the number of bits, using Hashed values led to Hash Collisions for lower numbers of nodes. Thus, it was decided that random numbers would be picked from a range as keys to have a stable algorithm across different execution inputs</li>
    <li>It was observed that the readings are stable but do not follow a trend. This is because we pick random keys as inputs while testing our implementation. Thus, in certain executions, nodes traverse farther for finding a key, whereas in some instances they traverse less. </li>
    <li>But the key observation is that this traversal is always bounded by log2(NumberOfNodes) with a certain margin deviation</li>
</ol>

****

## Execution Screenshots for Largest Network

<img src="Execution - 1400 & 100.PNG"/>