#!/bin/bash

# all connected graphs with 5 nodes
geng -c 5 graph5c.g6

# regular 4 graphs with 5-14 nodes
geng -c -d4 -D4 5 reg4c5.g6
geng -c -d4 -D4 6 reg4c6.g6
geng -c -d4 -D4 7 reg4c7.g6
geng -c -d4 -D4 8 reg4c8.g6
geng -c -d4 -D4 9 reg4c9.g6
geng -c -d4 -D4 10 reg4c10.g6
geng -c -d4 -D4 11 reg4c11.g6
geng -c -d4 -D4 12 reg4c12.g6
geng -c -d4 -D4 13 reg4c13.g6
geng -c -d4 -D4 14 reg4c14.g6

# filter regular 4 graphs with diameter >= 4
pickg -Z4 reg4c13.g6 reg4c13dia4.g6
pickg -Z4 reg4c14.g6 reg4c14dia4.g6
pickg -Z5 reg4c13.g6 reg4c13dia5.g6
pickg -Z5 reg4c14.g6 reg4c14dia5.g6

# connected 8 graphs, diameter >= 4
geng -c -q 8 | pickg -Z4: -q > graph8cdia4p.g6
