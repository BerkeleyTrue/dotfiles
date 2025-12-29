#!/bin/bash
query=$(cat | jq -r '.query')
cd "$CLAUDE_PROJECT_DIR" && fd -t f -p "$query" . | sed 's|^\./||' | head -15
