#!/usr/bin/liquidsoap

# Asteroid Radio - Simple streaming script
# Streams music library continuously to Icecast2

# Set log level for debugging
settings.log.level := 4

# Create playlist source - use single_track to test first
# radio = single("/home/glenn/Projects/Code/asteroid/music/library/03-Driving.mp3")

# Create playlist from directory (simpler approach)
radio = playlist(mode="randomize", reload=3600, "/home/glenn/Projects/Code/asteroid/music/library/")

# Add some processing
radio = amplify(1.0, radio)

# Make source safe with fallback but prefer the music
radio = fallback(track_sensitive=false, [radio, sine(440.0)])

# Output to Icecast2
output.icecast(
  %mp3(bitrate=128),
  host="localhost",
  port=8000,
  password="H1tn31EhsyLrfRmo",
  mount="asteroid.mp3",
  name="Asteroid Radio",
  description="Music for Hackers - Streaming from the Asteroid",
  genre="Electronic/Alternative",
  url="http://localhost:8080/asteroid/",
  radio
)

print("🎵 Asteroid Radio streaming started!")
print("Stream URL: http://localhost:8000/asteroid.mp3")
print("Admin panel: http://localhost:8000/admin/")
