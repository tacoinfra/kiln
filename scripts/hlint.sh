#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2020 Tocqueville Group
#
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# To make tput work in gitlab runner
export TERM="${TERM:-xterm}"

find . \
  -path '*/\.*' -prune -o \
  -name '*.hs' \
  -exec hlint --hint .hlint.yaml --ignore='Parse error' {} +

ex=$?

if [ $ex != 0 ]; then
  echo ''
  echo '====================================================================='
  echo ''
  echo 'Note: to ignore a particular hint (e.g. "Reduce duplication"), write'
  echo 'this in the source file:'
  echo ''
  tput bold
  echo '{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''
  echo 'You can also apply it just to a particular function, which is better:'
  echo ''
  tput bold
  echo '{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''

  exit $ex
fi
