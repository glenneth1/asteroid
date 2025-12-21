-- Migration 008: User Playlists
-- Adds table for user-created playlists with submission/review workflow

CREATE TABLE IF NOT EXISTS user_playlists (
    _id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES "USERS"(_id) ON DELETE CASCADE,
    name TEXT NOT NULL,
    description TEXT,
    track_ids TEXT DEFAULT '[]',  -- JSON array of track IDs
    status TEXT DEFAULT 'draft' CHECK (status IN ('draft', 'submitted', 'approved', 'rejected', 'scheduled')),
    created_date INTEGER DEFAULT EXTRACT(EPOCH FROM NOW())::INTEGER,
    submitted_date INTEGER,
    reviewed_date INTEGER,
    reviewed_by INTEGER REFERENCES "USERS"(_id),
    review_notes TEXT
);

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_user_playlists_user_id ON user_playlists(user_id);
CREATE INDEX IF NOT EXISTS idx_user_playlists_status ON user_playlists(status);

-- Grant permissions
GRANT ALL PRIVILEGES ON user_playlists TO asteroid;
GRANT ALL PRIVILEGES ON SEQUENCE user_playlists__id_seq TO asteroid;

-- Verification
DO $$
BEGIN
    RAISE NOTICE 'Migration 008: User playlists table created successfully!';
END $$;
