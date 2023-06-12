#!/usr/bin/env bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

interpreter=lagger
all_good=$(find examples/good -type f -name "*.lagger" | wc -l)
all_bad=$(find examples/bad -type f -name "*.lagger" | wc -l)
all=$((all_good + all_bad))

make

echo -e " \n${YELLOW}== Running examples. ==${NC}"
echo -e " \n${YELLOW}== Running good examples. All should pass. ==${NC}\n"

passed_good=0
for example in examples/good/*.lagger; do
  echo -e "${YELLOW}$example:${NC}\n"
  ./$interpreter "$example"

  if [[ $? -eq 0 ]]; then
    ((passed_good++))
    echo -e "${GREEN}\n-- OK --${NC}\n\n"
  else
    echo -e "${RED}\n-- WA --${NC}\n\n"
  fi
done

echo -e "\n${YELLOW}== Running bad examples. All should fail. ==${NC}\n"

passed_bad=0
for example in examples/bad/*.lagger; do
  echo -e "${YELLOW}$example:${NC}\n"
  ./$interpreter "$example"

  if [[ $? -eq 1 ]]; then
    ((passed_bad++))
    echo -e "${GREEN}\n-- OK --${NC}\n\n"
  else
    echo -e "${RED}\n-- WA --${NC}\n\n"
  fi
done

passed_all=$((passed_good + passed_bad))
echo -e "${YELLOW}== Summary: ==${NC}\n"
echo -e "Good passed: ${GREEN}$passed_good / $all_good${NC}"
echo -e "Bad passed:  ${GREEN}$passed_bad / $all_bad${NC}"
echo -e "All passed:  ${GREEN}$passed_all / $all${NC}"
