-- Migration 006: User Avatars
-- Adds avatar support to user profiles

-- Add avatar_path column to USERS table
ALTER TABLE "USERS" ADD COLUMN IF NOT EXISTS avatar_path TEXT;

-- Grant permissions
GRANT ALL PRIVILEGES ON "USERS" TO asteroid;

-- Verification
DO $$
BEGIN
    RAISE NOTICE 'Migration 006: User avatars column added successfully!';
END $$;
