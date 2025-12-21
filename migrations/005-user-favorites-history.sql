-- Migration 005: User Favorites and Listening History
-- Adds tables for track favorites/ratings and per-user listening history
-- Updated to support title-based storage (no tracks table dependency)

-- User favorites table - tracks that users have liked/rated
-- Supports both track-id (when tracks table is populated) and track_title (for now)
CREATE TABLE IF NOT EXISTS user_favorites (
    _id SERIAL PRIMARY KEY,
    "user-id" INTEGER NOT NULL REFERENCES "USERS"(_id) ON DELETE CASCADE,
    "track-id" INTEGER,  -- Optional: references tracks(_id) when available
    track_title TEXT,    -- Store title directly for title-based favorites
    rating INTEGER DEFAULT 1 CHECK (rating >= 1 AND rating <= 5),
    "created-date" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_user_favorites_user_id ON user_favorites("user-id");
CREATE INDEX IF NOT EXISTS idx_user_favorites_track_id ON user_favorites("track-id");
CREATE INDEX IF NOT EXISTS idx_user_favorites_rating ON user_favorites(rating);
CREATE UNIQUE INDEX IF NOT EXISTS idx_user_favorites_unique ON user_favorites("user-id", COALESCE(track_title, ''));

-- User listening history - per-user track play history
-- Supports both track-id and track_title
CREATE TABLE IF NOT EXISTS listening_history (
    _id SERIAL PRIMARY KEY,
    "user-id" INTEGER NOT NULL REFERENCES "USERS"(_id) ON DELETE CASCADE,
    "track-id" INTEGER,  -- Optional: references tracks(_id) when available
    track_title TEXT,    -- Store title directly for title-based history
    "listened-at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "listen-duration" INTEGER DEFAULT 0,  -- seconds listened
    completed INTEGER DEFAULT 0           -- 1 if they listened to the whole track
);

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_listening_history_user_id ON listening_history("user-id");
CREATE INDEX IF NOT EXISTS idx_listening_history_track_id ON listening_history("track-id");
CREATE INDEX IF NOT EXISTS idx_listening_history_listened_at ON listening_history("listened-at");

-- Grant permissions
GRANT ALL PRIVILEGES ON user_favorites TO asteroid;
GRANT ALL PRIVILEGES ON listening_history TO asteroid;
GRANT ALL PRIVILEGES ON SEQUENCE user_favorites__id_seq TO asteroid;
GRANT ALL PRIVILEGES ON SEQUENCE listening_history__id_seq TO asteroid;

-- Verification
DO $$
BEGIN
    RAISE NOTICE 'Migration 005: User favorites and listening history tables created successfully!';
END $$;
