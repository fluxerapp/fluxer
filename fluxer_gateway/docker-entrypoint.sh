#!/bin/bash

# Copyright (C) 2026 Fluxer Contributors
#
# This file is part of Fluxer.
#
# Fluxer is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fluxer is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

set -e

# Set default values for environment variables if not set
export LOGGER_LEVEL=${LOGGER_LEVEL:-debug}
export RELEASE_NODE=${RELEASE_NODE:-fluxer_gateway@gateway}

# Substitute environment variables in config files
envsubst < /workspace/config/vm.args.template > /workspace/config/vm.args
envsubst < /workspace/config/sys.config.template > /workspace/config/sys.config

# Start inotify watcher in the background for auto-recompilation
(while true; do
    inotifywait -r -e modify,create,delete,move src/ config/ 2>/dev/null && \
    sleep 0.5 && \
    rebar3 compile && \
    envsubst < /workspace/config/vm.args.template > /workspace/config/vm.args && \
    envsubst < /workspace/config/sys.config.template > /workspace/config/sys.config
done) &

# Start the Erlang application
exec erl -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -args_file config/vm.args \
    -eval 'application:ensure_all_started(fluxer_gateway)' \
    -noshell
