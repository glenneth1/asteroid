# Fix frontend JavaScript to work with define-api endpoints

## Summary
Updates all frontend JavaScript files to properly work with the refactored `define-api` endpoints, including API path corrections and RADIANCE response format handling.

## Changes Made

### API Endpoint Path Updates
- **Fixed API paths** in `users.js` and `profile.js` from `/asteroid/api/` to `/api/asteroid/`
- **Corrected endpoint references** to match new `define-api` URL structure
- **Updated all fetch calls** to use consistent API path format

### RADIANCE API Response Handling
- **Added response wrapper handling** for RADIANCE's `{status: 200, data: {...}}` format
- **Implemented fallback logic** with `result.data || result` pattern
- **Fixed icecast-status parsing** in both `front-page.js` and `player.js`
- **Enhanced error handling** for API response processing

### JavaScript Fixes
- **Fixed missing function declaration** in `player.js` (`loadTracks` function)
- **Improved error handling** in track loading with proper user feedback
- **Added debug logging** for API response troubleshooting
- **Enhanced live metadata updates** for real-time track information

### Files Updated
- **`static/js/front-page.js`** - Fixed icecast-status response parsing for live metadata
- **`static/js/player.js`** - Fixed function declaration and RADIANCE response handling  
- **`static/js/users.js`** - Updated all API paths from `/asteroid/api/` to `/api/asteroid/`
- **`static/js/profile.js`** - Updated all API paths from `/asteroid/api/` to `/api/asteroid/`

## Key Improvements

### Live Metadata Integration
- **Real-time track updates** now working correctly on front page
- **Automatic refresh** every 10 seconds showing current playing track
- **Proper parsing** of "Artist - Track" format from Icecast
- **Fallback handling** for missing or malformed metadata

### Enhanced User Experience
- **Consistent API communication** across all frontend components
- **Better error messaging** when API calls fail
- **Improved loading states** and user feedback
- **Seamless integration** with authentication system

## Testing
- ✅ **All 18 tests pass** via `./test-server.sh`
- ✅ **Live metadata working** - Shows actual track names instead of "The Void - Silence"
- ✅ **Frontend API calls successful** - All JavaScript fetch operations working
- ✅ **User interface responsive** - All interactive elements functioning
- ✅ **Authentication integration** - Proper handling of protected endpoints

## Technical Details
- **RADIANCE API wrapper**: Properly handles `{status: 200, message: "Ok", data: {...}}` format
- **Error resilience**: Graceful fallback when API responses are malformed
- **Consistent patterns**: Standardized approach to API response handling across all files
- **Debug support**: Added logging for troubleshooting API communication

## Impact
- **Fully functional frontend** working with refactored backend APIs
- **Live streaming metadata** displaying real track information
- **Enhanced user experience** with proper error handling and feedback
- **Maintainable codebase** with consistent API communication patterns

This update completes the frontend integration with the `define-api` refactoring, ensuring all user-facing functionality works seamlessly with the new backend API structure.
