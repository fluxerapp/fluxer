#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Refreshes yt-dlp into the persistent volume, because YouTube changes often.
# On any failure the bundled copy in /opt/ytdlp is used instead.

VENV=/var/lib/ytdlp/venv

if [ ! -x "$VENV/bin/pip" ]; then
	timeout 90 python3 -m venv --upgrade-deps "$VENV" || echo "[music] could not create the yt-dlp venv"
fi

if [ -x "$VENV/bin/pip" ] && timeout 90 "$VENV/bin/pip" install -U 'yt-dlp[default]'; then
	export YTDLP_BIN="$VENV/bin/yt-dlp"
else
	echo "[music] yt-dlp update failed, using the bundled copy"
	export YTDLP_BIN=/opt/ytdlp/bin/yt-dlp
fi

exec node dist/Main.js
