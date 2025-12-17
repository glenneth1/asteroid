-- Migration 004: Playlist Schedule Table
-- Description: Store playlist schedule configuration in database for persistence

CREATE TABLE IF NOT EXISTS playlist_schedule (
    _id SERIAL PRIMARY KEY,
    hour INTEGER NOT NULL UNIQUE CHECK (hour >= 0 AND hour <= 23),
    playlist VARCHAR(255) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert default schedule
INSERT INTO playlist_schedule (hour, playlist) VALUES
    (0, 'midnight-ambient.m3u'),
    (6, 'morning-drift.m3u'),
    (12, 'afternoon-orbit.m3u'),
    (18, 'evening-descent.m3u')
ON CONFLICT (hour) DO NOTHING;

-- Grant permissions
GRANT ALL PRIVILEGES ON playlist_schedule TO asteroid;
GRANT ALL PRIVILEGES ON playlist_schedule__id_seq TO asteroid;

DO $$
BEGIN
    RAISE NOTICE 'Playlist schedule table created successfully!';
END $$;
