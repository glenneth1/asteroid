# Convert JavaScript to Parenscript with Enhanced Spectrum Analyzer

## Overview

This PR converts all frontend JavaScript files to Parenscript (Common Lisp that compiles to JavaScript), improving maintainability and code consistency across the Asteroid Radio codebase. Additionally, it includes significant enhancements to the spectrum analyzer with theming, multiple visualization styles, and improved stream reliability.

## Major Changes

### 1. Parenscript Conversion

- **Converted 7 JavaScript files to Parenscript:**
  - `admin.js` → `parenscript/admin.lisp`
  - `auth-ui.js` → `parenscript/auth-ui.lisp`
  - `front-page.js` → `parenscript/front-page.lisp`
  - `player.js` → `parenscript/player.lisp`
  - `profile.js` → `parenscript/profile.lisp`
  - `users.js` → `parenscript/users.lisp`
  - `recently-played.js` → `parenscript/recently-played.lisp`

- **Original JavaScript files** renamed to `.js.original` for reference
- **Added** `parenscript-utils.lisp` for shared compilation utilities
- **Parenscript files are now the source of truth**; JavaScript is generated dynamically on request

### 2. Stream Reliability Improvements

- **Automatic reconnection logic** for live stream after long pauses (>10 seconds)
- **Improved spectrum analyzer** audio context handling with proper reset functionality
- **Fixed stream pause detection** to prevent unnecessary "now playing" updates during paused playback
- **Better error handling** for stream stalls and connection issues

### 3. Spectrum Analyzer Enhancements

#### Color Themes (6 options)
- **Monotone** - Deep blue to cobalt gradient matching site aesthetic
- **Green** - Classic bright green gradient (default)
- **Blue** - Cyan to deep blue
- **Purple** - Magenta to deep purple
- **Red** - Bright red to dark red
- **Amber** - Orange to brown

#### Visualization Styles (3 options)
- **Bars** - Traditional bar graph (default)
- **Wave** - Continuous line/waveform
- **Dots** - Particle/dot visualization

#### UI Features
- **Dropdown controls** for real-time theme and style selection
- **Persistent preferences** saved to localStorage
- **Dynamic border color** that changes to match selected theme
- **Dynamic dropdown styling** - dropdown boxes update their colors to match the selected theme
- **Mute detection** with visual "MUTED" indicator overlay

### 4. UX Improvements

- **Live indicator animation** changed from blinking to smooth pulse (inspired by old MacBook sleep indicator)
- **Removed verbose debug logging** from console output
- **Cleaner, more professional** user experience throughout

### 5. Documentation & Scripts

- **Added** `docs/PARENSCRIPT-EXPERIMENT.org` documenting the conversion process and rationale
- **Added helper scripts** for music library management:
  - `scripts/music-library-tree.py` - Generate library tree structure
  - `scripts/fix-m3u-paths.py` - Fix playlist paths
  - `scripts/scan.py` - Library scanning utilities
- **Added sample playlists** and playlist documentation

## Technical Details

### Parenscript Integration

- All Parenscript files compile to JavaScript **on-the-fly** via custom route handlers in `asteroid.lisp`
- Compilation happens at request time, allowing for rapid development iteration
- No build step required for JavaScript changes
- Full access to Common Lisp macros and language features for frontend code

### Backward Compatibility

- ✅ Maintains full backward compatibility with existing functionality
- ✅ No breaking changes to API or user-facing features
- ✅ All existing routes and endpoints remain unchanged
- ✅ Original JavaScript preserved as `.original` files for reference

### Code Quality

- Removed generated `asteroid.css` from repository (auto-generated on build)
- Consistent code style across frontend and backend (all Lisp)
- Better type safety and error handling through Lisp's condition system
- Easier refactoring and maintenance with unified language

## Testing

- ✅ All existing functionality verified working
- ✅ Stream reconnection tested with long pauses (>10 seconds)
- ✅ Spectrum analyzer themes and styles tested across all pages (front page, player, admin)
- ✅ Preferences persistence verified across browser sessions
- ✅ Cross-browser compatibility maintained
- ✅ Audio context handling tested with multiple audio elements
- ✅ Mute detection working correctly

## Migration Notes

### For Developers

- Frontend changes should now be made in `parenscript/*.lisp` files
- JavaScript is generated automatically - no need to edit `.js` files
- Use `make clean && make` to rebuild after Parenscript changes
- Original JavaScript files kept as `.js.original` for reference during transition

### For Deployment

- No changes to deployment process
- Server generates JavaScript on-the-fly from Parenscript
- No additional dependencies beyond existing Common Lisp setup

## Files Changed

### Added
- `parenscript-utils.lisp` - Parenscript compilation utilities
- `parenscript/admin.lisp` - Admin interface logic
- `parenscript/auth-ui.lisp` - Authentication UI
- `parenscript/front-page.lisp` - Front page and live stream
- `parenscript/player.lisp` - Audio player controls
- `parenscript/profile.lisp` - User profile management
- `parenscript/recently-played.lisp` - Recently played tracks
- `parenscript/users.lisp` - User management
- `docs/PARENSCRIPT-EXPERIMENT.org` - Documentation
- `scripts/*` - Helper scripts for library management

### Modified
- `asteroid.lisp` - Added Parenscript compilation routes, removed debug logging
- `frontend-partials.lisp` - Removed debug logging from Icecast stats
- `static/asteroid.lass` - Updated live indicator animation
- `template/*.ctml` - Added spectrum analyzer controls

### Renamed
- `static/js/admin.js` → `static/js/admin.js.original`
- `static/js/auth-ui.js` → `static/js/auth-ui.js.original`
- `static/js/front-page.js` → `static/js/front-page.js.original`
- `static/js/player.js` → `static/js/player.js.original`
- `static/js/profile.js` → `static/js/profile.js.original`
- `static/js/users.js` → `static/js/users.js.original`
- `static/js/recently-played.js` → `static/js/recently-played.js.original`

### Deleted
- `static/asteroid.css` - Now auto-generated, removed from repository

## Screenshots

The spectrum analyzer now features:
- Multiple color themes with dynamic border matching
- Three distinct visualization styles (bars, wave, dots)
- Clean dropdown UI for easy customization
- Smooth animations and transitions

## Future Enhancements

Potential future improvements building on this foundation:
- Additional visualization styles (spectrogram, circular, etc.)
- More color themes
- Customizable color picker for user-defined themes
- Visualization presets tied to music genres
- WebGL-accelerated rendering for complex visualizations

## Conclusion

This PR represents a significant modernization of the Asteroid Radio frontend while maintaining full backward compatibility. The move to Parenscript unifies the codebase under Common Lisp, making it easier to maintain and extend. The enhanced spectrum analyzer provides users with a more engaging and customizable listening experience.

**Ready to merge** ✅
