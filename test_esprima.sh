#!/bin/zsh
TESTS_RUN=0
TESTS_FAILED=0

cargo build

for SRC in esprima/test/fixtures/{JSX,expression}/**/*.js; do
    TESTS_RUN=$((TESTS_RUN+1))

    SRC_TEXT=$(cat $SRC)
    SRC_TREE=$(./target/debug/es "$SRC_TEXT" 2> /dev/null)

    if [ $? -eq "1" ]; then
        echo -e "\e[31m$SRC: failed\e[0m"
        echo "$SRC_TEXT"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    else
        echo -e "\e[32m$SRC: passed\e[0m"
        echo "$SRC_TEXT"
    fi

    #OUT="${SRC%.js}.json"
done

echo Tests Failed: "$TESTS_FAILED / $TESTS_RUN"
