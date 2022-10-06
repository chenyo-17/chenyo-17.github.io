---
title: "Load balancing in P4"
date: 2022-10-04 20:00:00 +0200
categories: p4
tags: p4 load-balancing
math: true
---

## What is load balancing

Load balancing refers to a set of strategies of packet forwarding to have optimal network utilization.

Shortest path routing strategies does not always lead to good utilization. 
For instance, when multiple switches choose the same shortest path to forward packet to the same destination, while there exists alternative path to the destination as well, then the alternative path is never used and the bandwidth on that path is wasted.

## How does ECMP allow load balancing

ECMP is a common protocol to allow multiple same best paths to be seleced randomly for different flows. 
In P4 switches, this can be allowed by configuring ECMP group for specific destinations, and then randomly select the next-hop based on the hash result of the packet header's hash.
One principle of choosing the hash field is to make sure packets from the same flow always chooses the same next-hop.

## Use flowlet switching to enhance ECMP

There are 2 limitations of using ECMP: hash collision and asymmetric topology.

First, in reality, the network traffic patterns tend to be: many flows carry small size traffic, which a small portion of flows carry burst traffic.
Hence, if there is hash collisions among burst flows, ECMP does not help load balancing.

Flowlet switching can be used to avoid such case.

In the flowlet switching, a random next-hop is selected for each flowlet, instead of each flow. 
A flowlet refers to a burst period within any flow, according to TCP mechanism, packets sent from the receiver wait for the receiver window space.
Therefore, pakcets waiting for the same window space form small bursts in the same flow, and a flow consists of such bursts with gaps in between.

Since packets from a later flowlet is unlikely to arrive earlier than packets from the earlier flowlet, it is safe to change next-hops for packets from different flowlets.

In P4, this can be implemented with registers.
One can use 2 registers.
The first is to record the timestamp of the last packet of each flow, and is used to compute the time difference with the current packet to decide whether they are from the same flowlet.
The second is to record and update the flowlet id of each flow, this id is used when hashing the packet header.
Each flow has an entry in both registers.
When a packet arrives, the switch first check the interval, if the interval is small, the switch reads the flowlet id and forward the packet based on the hash result.
Otherwise, the switch updates the flowlet id and forwards it to a new next-hop.


The second limitation of ECMP is due to topology asymmetry.
Assume the ECMP forwarding is enabled, and a switch takes path A and B to send traffic T1. 
All links on A and B have enough bankwidth to carry the entire traffic individually.
Assume another switch takes path C to send another traffic T2, and there is link sharing between A and C.
In this case, the load balancing on A and B is not an optimal choice, because this may decrease the throughput for traffic on path C,
as it needs to share bandwidth with T1, while T1 can use path B and leave more for T2.

To mitigate this issue, refer to [this paper](https://people.csail.mit.edu/alizadeh/papers/conga-sigcomm14.pdf). 
