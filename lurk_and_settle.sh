#!/bin/bash

clear
echo "Monitoring src/ for changes..."
echo "Pass 'b' to build (default) or 't' to test"

case $1 in
  t) CMD="dune test" ;;
  *) CMD="dune build" ;;
esac

inotifywait -q -m -r -e modify src | while read -r _DIRECTORY EVENT _FILE; do
  # echo $DIRECTORY $EVENT $FILE
  case $EVENT in
    MODIFY*)
      clear
      echo "= ${CMD} ==============================="; echo
      ${CMD}
      echo; echo "= $(date) ==================="
      ;;
  esac
done
