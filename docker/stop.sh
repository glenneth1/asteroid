#!/bin/bash

# Simple stop script for Docker directory
# Run from: /home/glenn/Projects/Code/asteroid/docker/

echo "🛑 Stopping Asteroid Radio Docker Services..."

# Stop services (postgres only - cl-streamer replaces Icecast + Liquidsoap)
docker compose down postgres
# docker compose down  # Uncomment to stop all services

# if we really need to clean everything and start fresh, run the
# following commands:

# docker compose down postgres
# docker volume rm docker_postgres-data

# TODO - apply a getopt interface to this script.


echo ""
echo "✅ Services stopped."
