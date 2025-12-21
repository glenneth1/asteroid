-- Migration 005: User Favorites and Listening History
-- Adds tables for track favorites/ratings and per-user listening history

-- User favorites table - tracks that users have liked/rated
CREATE TABLE IF NOT EXISTS user_favorites (
    _id SERIAL PRIMARY KEY,
    "user-id" INTEGER NOT NULL REFERENCES "USERS"(_id) ON DELETE CASCADE,
    "track-id" INTEGER NOT NULL REFERENCES tracks(_id) ON DELETE CASCADE,
    rating INTEGER DEFAULT 1 CHECK (rating >= 1 AND rating <= 5),
    "created-date" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE("user-id", "track-id")
);

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_user_favorites_user_id ON user_favorites("user-id");
CREATE INDEX IF NOT EXISTS idx_user_favorites_track_id ON user_favorites("track-id");
CREATE INDEX IF NOT EXISTS idx_user_favorites_rating ON user_favorites(rating);

-- User listening history - per-user track play history
CREATE TABLE IF NOT EXISTS listening_history (
    _id SERIAL PRIMARY KEY,
    "user-id" INTEGER NOT NULL REFERENCES "USERS"(_id) ON DELETE CASCADE,
    "track-id" INTEGER NOT NULL REFERENCES tracks(_id) ON DELETE CASCADE,
    "listened-at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "listen-duration" INTEGER DEFAULT 0,  -- seconds listened
    "completed" BOOLEAN DEFAULT false     -- did they listen to the whole track?
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
