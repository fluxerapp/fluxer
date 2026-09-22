#!/bin/bash

LEGACY_DIR='/opt/Fluxer Canary'
if [ -d "$LEGACY_DIR" ]; then
    if [ -z "$(find "$LEGACY_DIR" ! -type d -print -quit 2>/dev/null)" ]; then
        find "$LEGACY_DIR" -depth -type d -exec rmdir {} + >/dev/null 2>&1 || true
    fi
fi

exit 0
