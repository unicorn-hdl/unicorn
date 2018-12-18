#!/bin/bash

./run.sh comments/test1 c.c
rm comments/test1
./run.sh comments/test2 c.c
rm comments/test2
./run.sh comments/test3 c.c
rm comments/test3

./run.sh creatingBuses/test1 c.c
rm comments/test1
./run.sh creatingBuses/test2 c.c
rm comments/test2
./run.sh creatingBuses/test3 c.c
rm comments/test3
./run.sh creatingBuses/test4 c.c
rm comments/test4
./run.sh creatingBuses/test5 c.c
rm comments/test5
./run.sh creatingBuses/test6 c.c
rm comments/test6
