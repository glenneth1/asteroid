#!/bin/bash

# Asteroid Radio - Stop Script
# Stops all services for internet radio streaming

echo "🛑 Stopping Asteroid Radio Station..."

# Function to stop a service
stop_service() {
    local service=$1
    local process_name=$2
    local use_sudo=$3
    
    if pgrep -f "$process_name" > /dev/null; then
        echo "⏳ Stopping $service..."
        if [ "$use_sudo" = "sudo" ]; then
            sudo pkill -f "$process_name"
        else
            pkill -f "$process_name"
        fi
        sleep 2
        
        if ! pgrep -f "$process_name" > /dev/null; then
            echo "✅ $service stopped"
        else
            echo "⚠️  $service may still be running"
        fi
    else
        echo "ℹ️  $service is not running"
    fi
}

# Stop Liquidsoap streaming
stop_service "Liquidsoap Streaming" "liquidsoap.*asteroid-radio.liq"

# Stop Asteroid web server
stop_service "Asteroid Web Server" "asteroid"

# Stop Icecast2
stop_service "Icecast2" "icecast2" "sudo"

echo ""
echo "🔇 Asteroid Radio services stopped"
echo "To restart, run: ./start-asteroid-radio.sh"
