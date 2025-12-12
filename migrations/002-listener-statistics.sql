-- Migration: Listener Statistics Tables
-- Version: 002
-- Date: 2025-12-08
-- Description: Add tables for tracking listener statistics with GDPR compliance

-- Listener snapshots: periodic counts from Icecast polling
CREATE TABLE IF NOT EXISTS listener_snapshots (
    _id SERIAL PRIMARY KEY,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    mount VARCHAR(100) NOT NULL,
    listener_count INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX IF NOT EXISTS idx_snapshots_timestamp ON listener_snapshots(timestamp);
CREATE INDEX IF NOT EXISTS idx_snapshots_mount ON listener_snapshots(mount);

-- Listener sessions: individual connection records (privacy-safe)
CREATE TABLE IF NOT EXISTS listener_sessions (
    _id SERIAL PRIMARY KEY,
    session_id VARCHAR(64) UNIQUE NOT NULL,
    session_start TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    session_end TIMESTAMP,
    ip_hash VARCHAR(64) NOT NULL,  -- SHA256 hash, not reversible
    country_code VARCHAR(2),
    city VARCHAR(100),
    region VARCHAR(100),
    user_agent TEXT,
    mount VARCHAR(100) NOT NULL,
    duration_seconds INTEGER,
    user_id INTEGER REFERENCES "USERS"(_id) ON DELETE SET NULL  -- Optional link to registered user
);

CREATE INDEX IF NOT EXISTS idx_sessions_start ON listener_sessions(session_start);
CREATE INDEX IF NOT EXISTS idx_sessions_ip_hash ON listener_sessions(ip_hash);
CREATE INDEX IF NOT EXISTS idx_sessions_country ON listener_sessions(country_code);
CREATE INDEX IF NOT EXISTS idx_sessions_user ON listener_sessions(user_id);

-- Daily aggregated statistics (for efficient dashboard queries)
CREATE TABLE IF NOT EXISTS listener_daily_stats (
    _id SERIAL PRIMARY KEY,
    date DATE NOT NULL,
    mount VARCHAR(100) NOT NULL,
    unique_listeners INTEGER DEFAULT 0,
    peak_concurrent INTEGER DEFAULT 0,
    total_listen_minutes INTEGER DEFAULT 0,
    new_listeners INTEGER DEFAULT 0,
    returning_listeners INTEGER DEFAULT 0,
    avg_session_minutes DECIMAL(10,2),
    UNIQUE(date, mount)
);

CREATE INDEX IF NOT EXISTS idx_daily_stats_date ON listener_daily_stats(date);

-- Hourly breakdown for time-of-day analysis
CREATE TABLE IF NOT EXISTS listener_hourly_stats (
    _id SERIAL PRIMARY KEY,
    date DATE NOT NULL,
    hour INTEGER NOT NULL CHECK (hour >= 0 AND hour <= 23),
    mount VARCHAR(100) NOT NULL,
    unique_listeners INTEGER DEFAULT 0,
    peak_concurrent INTEGER DEFAULT 0,
    UNIQUE(date, hour, mount)
);

CREATE INDEX IF NOT EXISTS idx_hourly_stats_date ON listener_hourly_stats(date);

-- Geographic aggregates
CREATE TABLE IF NOT EXISTS listener_geo_stats (
    _id SERIAL PRIMARY KEY,
    date DATE NOT NULL,
    country_code VARCHAR(2) NOT NULL,
    city VARCHAR(100),
    listener_count INTEGER DEFAULT 0,
    listen_minutes INTEGER DEFAULT 0,
    UNIQUE(date, country_code)
);

CREATE INDEX IF NOT EXISTS idx_geo_stats_date ON listener_geo_stats(date);
CREATE INDEX IF NOT EXISTS idx_geo_stats_country ON listener_geo_stats(country_code);

-- User listening history (for registered users only)
CREATE TABLE IF NOT EXISTS user_listening_history (
    _id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES "USERS"(_id) ON DELETE CASCADE,
    track_title VARCHAR(500),
    track_artist VARCHAR(500),
    listened_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    duration_seconds INTEGER
);

CREATE INDEX IF NOT EXISTS idx_user_history_user ON user_listening_history(user_id);
CREATE INDEX IF NOT EXISTS idx_user_history_listened ON user_listening_history(listened_at);

-- Data retention: function to clean old session data (GDPR compliance)
CREATE OR REPLACE FUNCTION cleanup_old_listener_data(retention_days INTEGER DEFAULT 30)
RETURNS INTEGER AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    -- Delete individual sessions older than retention period
    DELETE FROM listener_sessions 
    WHERE session_start < NOW() - (retention_days || ' days')::INTERVAL;
    
    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    
    RAISE NOTICE 'Cleaned up % listener session records older than % days', deleted_count, retention_days;
    
    RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

-- Grant permissions
GRANT ALL PRIVILEGES ON listener_snapshots TO asteroid;
GRANT ALL PRIVILEGES ON listener_sessions TO asteroid;
GRANT ALL PRIVILEGES ON listener_daily_stats TO asteroid;
GRANT ALL PRIVILEGES ON listener_hourly_stats TO asteroid;
GRANT ALL PRIVILEGES ON listener_geo_stats TO asteroid;
GRANT ALL PRIVILEGES ON user_listening_history TO asteroid;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO asteroid;

-- Success message
DO $$
BEGIN
    RAISE NOTICE 'Listener statistics tables created successfully!';
    RAISE NOTICE 'Tables: listener_snapshots, listener_sessions, listener_daily_stats, listener_hourly_stats, listener_geo_stats, user_listening_history';
    RAISE NOTICE 'GDPR: IP addresses are hashed, cleanup function available';
END $$;
