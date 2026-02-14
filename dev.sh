#!/bin/bash

# Development script for running server and client together

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR/server"

# Function to cleanup background processes on exit
cleanup() {
    echo -e "\n${BLUE}Shutting down...${NC}"
    if [ ! -z "$VITE_PID" ]; then
        kill $VITE_PID 2>/dev/null || true
    fi
    if [ ! -z "$SERVER_PID" ]; then
        kill $SERVER_PID 2>/dev/null || true
    fi
    exit 0
}

trap cleanup EXIT INT TERM

# Do initial build
echo -e "${GREEN}Building UI assets...${NC}"
npm run build

# Start Vite in build watch mode
echo -e "${GREEN}Starting Vite build watcher...${NC}"
npm run dev &
VITE_PID=$!

# Build and run server
echo -e "${GREEN}Building Haskell server...${NC}"
cabal build

echo -e "${GREEN}Starting server...${NC}"
SERVER_BIN=$(cabal list-bin server)
$SERVER_BIN --player guest &
SERVER_PID=$!

echo -e "${BLUE}Server running at http://localhost:8080?player=guest${NC}"
echo -e "${BLUE}Press Ctrl+C to stop${NC}"

# Wait for both processes
wait
