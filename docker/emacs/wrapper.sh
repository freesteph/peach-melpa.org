#!/bin/sh

Xvfb :13 -screen 0 "1600x1200x24+32" &

echo "started Xvfb server on DISPLAY=:13"

exec "$@"
