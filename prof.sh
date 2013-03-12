#!/bin/sh
set -e

make map-rope
./map-rope 64m 4096 +RTS -N2 -s -hd -i0.05
hp2ps -c map-rope
evince map-rope.ps
