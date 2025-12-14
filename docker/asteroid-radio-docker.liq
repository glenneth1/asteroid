#!/usr/bin/liquidsoap

# Asteroid Radio - Docker streaming script
# Streams music library continuously to Icecast2 running in Docker

# Allow running as root in Docker
set("init.allow_root", true)

# Set log level (4 = warning, suppresses info messages including telnet noise)
log.level.set(4)

# Audio buffering settings to prevent choppiness
settings.frame.audio.samplerate.set(44100)
settings.frame.audio.channels.set(2)
# Use "fast" resampler instead of "best" to reduce CPU load on 96kHz files
settings.audio.converter.samplerate.libsamplerate.quality.set("fast")

# Prefer native decoders over FFmpeg for better performance
settings.decoder.priorities.flac := 10
settings.decoder.priorities.mad := 10
settings.decoder.priorities.ffmpeg := 1

# Enable telnet server for remote control
settings.server.telnet.set(true)
settings.server.telnet.port.set(1234)
settings.server.telnet.bind_addr.set("0.0.0.0")

# =============================================================================
# CURATED STREAM (Low Orbit) - Sequential playlist
# =============================================================================

# Create playlist source from generated M3U file
# This file is managed by Asteroid's stream control system
# Falls back to directory scan if playlist file doesn't exist
radio = playlist(
  mode="normal",      # Normal mode: play sequentially without initial randomization
  reload=300,         # Check for playlist updates every 5 minutes
  reload_mode="watch",  # Watch file for changes (more efficient than polling)
  "/app/stream-queue.m3u"
)

# Fallback to directory scan if playlist file is empty/missing
radio_fallback = playlist.safe(
  mode="randomize",
  reload=3600,
  "/app/music/"
)

# Use main playlist, fall back to directory scan
radio = fallback(track_sensitive=false, [radio, radio_fallback])

# Simple crossfade for smooth transitions
radio = crossfade(
  duration=3.0,        # 3 second crossfade
  fade_in=2.0,         # 2 second fade in
  fade_out=2.0,        # 2 second fade out
  radio
)

# Add buffer after crossfade to handle high sample rate files (96kHz -> 44.1kHz resampling)
radio = buffer(buffer=5.0, max=10.0, radio)

# Create a fallback with emergency content
emergency = sine(440.0)
emergency = amplify(0.1, emergency)

# Make source safe with fallback
radio = fallback(track_sensitive=false, [radio, emergency])

# Add metadata
radio = map_metadata(fun(m) -> 
  [("title", m["title"] ?? "Unknown Track"),
   ("artist", m["artist"] ?? "Unknown Artist"),
   ("album", m["album"] ?? "Unknown Album")], radio)

# Output to Icecast2 (using container hostname)
output.icecast(
  %mp3(bitrate=128),
  host="icecast",  # Docker service name
  port=8000,
  password="H1tn31EhsyLrfRmo",
  mount="asteroid.mp3",
  name="Asteroid Radio",
  description="Music for Hackers - Streaming from the Asteroid",
  genre="Electronic/Alternative",
  url="http://localhost:8080/asteroid/",
  public=true,
  radio
)

# AAC High Quality Stream (96kbps - better quality than 128kbps MP3)
output.icecast(
  %fdkaac(bitrate=96),
  host="icecast",
  port=8000,
  password="H1tn31EhsyLrfRmo",
  mount="asteroid.aac",
  name="Asteroid Radio (AAC)",
  description="Music for Hackers - High efficiency AAC stream",
  genre="Electronic/Alternative",
  url="http://localhost:8080/asteroid/",
  public=true,
  radio
)

# Low Quality MP3 Stream (for compatibility)
output.icecast(
  %mp3(bitrate=64),
  host="icecast",
  port=8000,
  password="H1tn31EhsyLrfRmo",
  mount="asteroid-low.mp3",
  name="Asteroid Radio (Low Quality)",
  description="Music for Hackers - Low bandwidth stream",
  genre="Electronic/Alternative",
  url="http://localhost:8080/asteroid/",
  public=true,
  radio
)

# =============================================================================
# SHUFFLE STREAM (Deep Space) - Random from full library
# =============================================================================

# Create shuffle source from full music library
shuffle_radio = playlist(
  mode="randomize",   # Random mode: shuffle tracks
  reload=3600,        # Reload playlist hourly
  "/app/music/"
)

# Apply crossfade for smooth transitions
shuffle_radio = crossfade(
  duration=3.0,
  fade_in=2.0,
  fade_out=2.0,
  shuffle_radio
)

# Add buffer to handle high sample rate files
shuffle_radio = buffer(buffer=5.0, max=10.0, shuffle_radio)

# Make source safe with emergency fallback
shuffle_radio = fallback(track_sensitive=false, [shuffle_radio, emergency])

# Add metadata
shuffle_radio = map_metadata(fun(m) -> 
  [("title", m["title"] ?? "Unknown Track"),
   ("artist", m["artist"] ?? "Unknown Artist"),
   ("album", m["album"] ?? "Unknown Album")], shuffle_radio)

# Shuffle Stream - Medium Quality MP3 (96kbps)
output.icecast(
  %mp3(bitrate=96),
  host="icecast",
  port=8000,
  password="H1tn31EhsyLrfRmo",
  mount="asteroid-shuffle.mp3",
  name="Asteroid Radio (Shuffle)",
  description="Music for Hackers - Random shuffle from the library",
  genre="Electronic/Alternative",
  url="http://localhost:8080/asteroid/",
  public=true,
  shuffle_radio
)

print("🎵 Asteroid Radio Docker streaming started!")
print("High Quality MP3:  http://localhost:8000/asteroid.mp3")
print("High Quality AAC:  http://localhost:8000/asteroid.aac")
print("Low Quality MP3:   http://localhost:8000/asteroid-low.mp3")
print("Shuffle Stream:    http://localhost:8000/asteroid-shuffle.mp3")
print("Icecast Admin:     http://localhost:8000/admin/")
print("Telnet control:    telnet localhost 1234")
