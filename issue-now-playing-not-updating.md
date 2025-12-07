# Issue: Now Playing Panel Not Updating on Player Page After Navigation

## Description
After implementing AJAX navigation for the frameset mode, the "Now Playing" panel on the player page (`/asteroid/player-content`) does not update with current track information. The panel remains empty or shows stale data even though the audio stream is playing correctly.

## Steps to Reproduce
1. Navigate to the frameset mode at `/asteroid/frameset`
2. Click on the "Player" navigation link to load the player page
3. Observe that the "Now Playing" panel does not populate with current track information
4. Navigate to another page and back to the player page
5. The panel still does not update

## Expected Behavior
The "Now Playing" panel should:
- Populate with current track information shortly after page load
- Update every 10 seconds with fresh data from the Icecast server
- Continue working after AJAX navigation between pages

## Root Cause
When content pages are loaded via AJAX using `document.write()`, the JavaScript files (including `player.js`) are being inserted into the DOM but not executed. The `<script>` tags are present in the HTML but the browser does not run them because they are added after the initial page load.

This means that:
- The `updateNowPlaying()` function is never called
- The `setInterval()` for periodic updates is never started
- Event listeners are not attached

## Proposed Solution
Modify the `loadInFrame()` function in all `-content.ctml` template files to manually re-execute scripts after loading new content via AJAX:

```javascript
// Execute scripts in the new content
const scripts = document.querySelectorAll('script');
scripts.forEach(oldScript => {
  const newScript = document.createElement('script');
  if (oldScript.src) {
    newScript.src = oldScript.src;
  } else {
    newScript.textContent = oldScript.textContent;
  }
  oldScript.parentNode.replaceChild(newScript, oldScript);
});
```

This approach:
1. Finds all `<script>` tags in the newly loaded content
2. Creates fresh script elements
3. Copies the source URL or inline content
4. Replaces the old (non-executed) scripts with new ones that the browser will execute

## Files to Modify
- `template/front-page-content.ctml`
- `template/player-content.ctml`
- `template/admin-content.ctml`
- `template/profile-content.ctml`
- `template/login-content.ctml`
- `template/status-content.ctml`

## Testing Plan
After implementing the fix:
1. Navigate to `/asteroid/frameset`
2. Click "Player" - verify the Now Playing panel populates within 1 second
3. Navigate to other pages and back - verify the panel continues to update correctly
4. Verify all dynamic content on all pages works as expected

## Related Issues
This fix should also resolve similar issues with other dynamic content that relies on JavaScript execution after page load.
