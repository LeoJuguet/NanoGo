#!/bin/sh

TYPING=$(find typing/*.go)
COMPILE=$(find compile/*.go)

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

echo "======================"
echo "========TYPING========"
echo "======================"

for FILE in $TYPING; do
    (../src/ngoc $FILE && echo -e "${RED}typing fail : ${FILE} ${NC}") \
        || echo -e "${GREEN}typing ok : ${FILE} ${NC}"
done;

echo "======================="
echo "========COMPILE========"
echo "======================="

for FILENAME in $COMPILE; do
    NAME=$(basename $FILENAME .go)
    (../src/ngoc ./compile/$NAME.go && echo -e "${GREEN}typing ok : ${NAME} ${NC}") \
        || echo -e "${RED}typing fail : ${NAME} ${NC}"
    gcc -no-pie -g ./compile/$NAME.s -o ./compile/$NAME.exe
    NGOC=$(./compile/$NAME.exe)
    GO=$(go run ./compile/$NAME.go 2> /dev/null)
    test "$NGOC" = "$GO" \
    && echo -e "${GREEN}compile ok : ${NAME} ${NC}" \
    || (echo -e "${RED}compile maybe fail : ${NAME} ${NC}";\
        paste -d '|' <(echo -e "$NGOC" ) <(echo -e "$GO") | \
        column -s '|' -t -N "NGOC, Go")
    rm ./compile/$NAME.{s,exe}
done;
