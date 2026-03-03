#!/bin/bash

# source our environment for credentials
. environment.sh

# Simple start script for Docker directory
# Run from: /home/glenn/Projects/Code/asteroid/docker/

echo "🎵 Starting Asteroid Radio Docker Services..."

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker first."
    exit 1
fi

# Start services (postgres only - cl-streamer replaces Icecast + Liquidsoap)
echo "🔧 Starting postgres..."
docker compose up -d postgres
# docker compose up -d  # Uncomment to start all services (Icecast + Liquidsoap)

# Wait and show status
sleep 3
echo ""
echo "📊 Service Status:"
docker compose ps

echo ""
echo "🎵 Asteroid Radio database is ready!"
echo "📡 Streaming is handled by cl-streamer (start from Lisp REPL)"
# Legacy Icecast URLs (no longer used):
# echo "📡 High Quality MP3: http://localhost:8000/asteroid.mp3"
# echo "📡 High Quality AAC: http://localhost:8000/asteroid.aac"
# echo "📡 Low Quality MP3:  http://localhost:8000/asteroid-low.mp3"
# echo "🔧 Admin Panel:      http://localhost:8000/admin/"
