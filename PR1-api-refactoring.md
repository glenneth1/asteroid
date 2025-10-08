# Refactor API endpoints to use Radiance's define-api macro

## Summary
Refactors all API endpoints from manual `define-page` implementations to use Radiance's built-in `define-api` macro, following framework best practices and conventions.

## Changes Made

### API Endpoint Refactoring
- **Converted 15+ API endpoints** from `define-page` to `define-api` format
- **Standardized URL patterns** to `/api/<module>/<endpoint>` structure
- **Implemented proper JSON responses** using `api-output` instead of manual JSON encoding
- **Added comprehensive error handling** with appropriate HTTP status codes (404, 500, 403)

### Key Technical Improvements
- **Automatic JSON formatting**: Leverages Radiance's built-in JSON API format
- **Parameter handling**: Uses lambda-list parameters instead of manual POST/GET parsing
- **Consistent error responses**: Standardized error format across all endpoints
- **Framework compliance**: Follows Radiance's recommended API patterns

### Endpoints Converted
- **Admin APIs**: `asteroid/admin/scan-library`, `asteroid/admin/tracks`
- **Playlist APIs**: `asteroid/playlists`, `asteroid/playlists/create`, `asteroid/playlists/add-track`, `asteroid/playlists/get`
- **Track APIs**: `asteroid/tracks`
- **Player APIs**: `asteroid/player/play`, `asteroid/player/pause`, `asteroid/player/stop`, `asteroid/player/resume`, `asteroid/player/status`
- **Status APIs**: `asteroid/status`, `asteroid/icecast-status`, `asteroid/auth-status`

### Added Comprehensive Test Suite
- **18 automated tests** covering all API endpoints
- **Response format validation** ensuring proper JSON output
- **Authentication testing** for protected endpoints
- **Static file serving verification**
- **HTML page loading tests**

## Testing
- ✅ **All 18 tests pass** via `./test-server.sh`
- ✅ **Proper JSON responses** instead of S-expressions
- ✅ **Authentication integration** working correctly
- ✅ **Error handling** with appropriate HTTP status codes
- ✅ **Backward compatibility** maintained for existing functionality

## Files Changed
- `asteroid.lisp` - Main API endpoint refactoring
- `auth-routes.lisp` - Authentication API endpoints (if applicable)
- Added comprehensive test suite

## Impact
- **Cleaner codebase** following Radiance framework conventions
- **Better maintainability** with standardized API patterns
- **Improved error handling** and status code management
- **Enhanced testing coverage** for all API functionality
- **Foundation for frontend integration** with consistent JSON responses

This refactoring establishes a solid foundation for frontend development and maintains full backward compatibility while improving code quality and framework compliance.
