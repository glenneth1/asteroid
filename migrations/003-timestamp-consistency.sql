-- Migration 003: Timestamp Consistency
-- Convert USERS table integer timestamps to TIMESTAMP type
-- This aligns USERS with tracks and playlists tables which already use TIMESTAMP

-- Step 1: Add new TIMESTAMP columns
ALTER TABLE "USERS" ADD COLUMN IF NOT EXISTS "created-date-new" TIMESTAMP;
ALTER TABLE "USERS" ADD COLUMN IF NOT EXISTS "last-login-new" TIMESTAMP;

-- Step 2: Convert existing epoch integers to timestamps
-- Only convert non-null values; epoch 0 or very old dates indicate no real value
UPDATE "USERS" 
SET "created-date-new" = TO_TIMESTAMP("created-date")
WHERE "created-date" IS NOT NULL 
  AND "created-date" > 0;

UPDATE "USERS"
SET "last-login-new" = TO_TIMESTAMP("last-login")
WHERE "last-login" IS NOT NULL
  AND "last-login" > 0;

-- Step 3: Drop old integer columns
ALTER TABLE "USERS" DROP COLUMN IF EXISTS "created-date";
ALTER TABLE "USERS" DROP COLUMN IF EXISTS "last-login";

-- Step 4: Rename new columns to original names
ALTER TABLE "USERS" RENAME COLUMN "created-date-new" TO "created-date";
ALTER TABLE "USERS" RENAME COLUMN "last-login-new" TO "last-login";

-- Step 5: Set default for created-date (new users get current timestamp)
ALTER TABLE "USERS" ALTER COLUMN "created-date" SET DEFAULT CURRENT_TIMESTAMP;

-- Verification query (run manually to check results):
-- SELECT _id, username, "created-date", "last-login" FROM "USERS";
