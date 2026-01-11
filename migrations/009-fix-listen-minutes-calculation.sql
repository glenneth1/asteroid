-- Migration: Fix Listen Minutes Calculation
-- Version: 009
-- Date: 2026-01-11
-- Description: Document the fix for listen_minutes calculation
--
-- ISSUE: listen_minutes was incrementing by 1 per poll regardless of listener count.
--        This meant a country with 5 listeners for 1 hour would only show 60 minutes,
--        not 300 listener-minutes.
--
-- FIX: Application code in listener-stats.lisp now increments listen_minutes by
--      the listener_count value (1 minute per listener per poll interval).
--
-- ADDITIONAL FIX: register-web-listener is now called from the now-playing-json API
--                 endpoint, keeping listeners registered during continuous playback
--                 instead of timing out after 5 minutes.
--
-- No schema changes required - this migration documents the application logic fix.

-- Add a comment to the table for future reference
COMMENT ON COLUMN listener_geo_stats.listen_minutes IS 
    'Total listener-minutes: increments by listener_count per poll (1 min per listener per 60s poll)';

-- Success message
DO $$
BEGIN
    RAISE NOTICE 'Migration 009: listen_minutes calculation fix documented';
    RAISE NOTICE 'listen_minutes now represents true listener-minutes (count * time)';
END $$;
