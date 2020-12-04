#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

$(dirname "${BASH_SOURCE[0]}")/{{release_name}} rpc cli_console run "$@"
