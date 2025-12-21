-- Migration 007: Track Request System
-- Allows users to request tracks for the stream with social attribution

-- Track requests table
CREATE TABLE IF NOT EXISTS track_requests (
    _id SERIAL PRIMARY KEY,
    "user-id" INTEGER NOT NULL REFERENCES "USERS"(_id) ON DELETE CASCADE,
    track_title TEXT NOT NULL,           -- Track title (Artist - Title format)
    track_path TEXT,                      -- Optional: path to file if known
    message TEXT,                         -- Optional message from requester
    status TEXT DEFAULT 'pending',        -- pending, approved, rejected, played
    "created-at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "reviewed-at" TIMESTAMP,              -- When admin reviewed
    "reviewed-by" INTEGER REFERENCES "USERS"(_id),
    "played-at" TIMESTAMP                 -- When it was actually played
);

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_track_requests_user_id ON track_requests("user-id");
CREATE INDEX IF NOT EXISTS idx_track_requests_status ON track_requests(status);
CREATE INDEX IF NOT EXISTS idx_track_requests_created ON track_requests("created-at");

-- Grant permissions
GRANT ALL PRIVILEGES ON track_requests TO asteroid;
GRANT ALL PRIVILEGES ON SEQUENCE track_requests__id_seq TO asteroid;

-- Verification
DO $$
BEGIN
    RAISE NOTICE 'Migration 007: Track requests table created successfully!';
END $$;
