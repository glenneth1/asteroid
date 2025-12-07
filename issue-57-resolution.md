# Resolution for Issue #57: Persistent Stream Playback Halts on Login Page Navigation

## Summary
This issue has been resolved. The persistent audio stream now continues playing uninterrupted when navigating to the login page and throughout the authentication flow in frameset mode.

## Original Problem
When using the frameset mode with the persistent audio player, navigating to the login page would halt the audio stream. This occurred because:
1. The login page was not frameset-aware (no `-content` version existed)
2. Navigation to `/login` would load the full page, replacing the frameset
3. This destroyed the persistent player frame, stopping the audio

## Solution Implemented

### 1. Created Frameset-Aware Login Page
Created `template/login-content.ctml` to be loaded within the content frame while preserving the persistent player frame.

### 2. Implemented AJAX Navigation
Modified all navigation links in frameset-aware pages to use AJAX loading via the `loadInFrame()` function:
```javascript
function loadInFrame(link) {
  const url = link.href;
  
  // Clear all intervals to prevent old page scripts from running
  const highestId = window.setTimeout(() => {}, 0);
  for (let i = 0; i < highestId; i++) {
    window.clearInterval(i);
    window.clearTimeout(i);
  }
  
  fetch(url)
    .then(response => response.text())
    .then(html => {
      document.open();
      document.write(html);
      document.close();
      
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
      
      // Re-initialize spectrum analyzer after navigation
      if (window.initializeSpectrumAnalyzer) {
        setTimeout(() => window.initializeSpectrumAnalyzer(), 100);
      }
      
      if (window.history && window.history.pushState) {
        window.history.pushState({}, '', url);
      }
    });
  
  return false;
}
```

### 3. Implemented AJAX Logout
Created `handleLogout()` function to perform logout without navigation:
```javascript
function handleLogout() {
  fetch('/asteroid/logout', {
    method: 'GET',
    redirect: 'manual'
  })
  .then(() => {
    // Reload the current page content to show logged-out state
    fetch(window.location.href)
      .then(response => response.text())
      .then(html => {
        document.open();
        document.write(html);
        document.close();
      });
  });
}
```

### 4. Updated Server-Side Logout Handler
Modified the logout route to detect frameset mode and redirect appropriately:
```lisp
(define-page logout #@"/logout" ()
  "Handle user logout"
  (setf (session:field "user-id") nil)
  ;; Check if we're in a frameset by looking at the Referer header
  (let* ((referer (radiance:header "Referer"))
         (in-frameset (and referer 
                          (or (search "/frameset" referer)
                              (search "/content" referer)
                              (search "-content" referer)))))
    (radiance:redirect (if in-frameset "/content" "/"))))
```

### 5. Created Additional Frameset-Aware Pages
- `template/admin-content.ctml` - Admin page for frameset mode
- `template/profile-content.ctml` - Profile page for frameset mode
- `template/status-content.ctml` - Status page placeholder for frameset mode

### 6. Added Route Handlers
Added corresponding route handlers in `asteroid.lisp`:
```lisp
(define-page login-content #@"/login-content" ()
  "Login page content (displayed in content frame)"
  (clip:process-to-string 
   (load-template "login-content")
   :title "🔐 Asteroid Radio - Login"))

(define-page status-content #@"/status-content" ()
  "Status page content (displayed in content frame)"
  (clip:process-to-string 
   (load-template "status-content")
   :title "📡 Asteroid Radio - Status"))
```

## Files Modified
- `auth-routes.lisp` - Updated logout handler to detect frameset mode
- `asteroid.lisp` - Added routes for login-content and status-content
- `template/login-content.ctml` - Created frameset-aware login page
- `template/admin-content.ctml` - Added AJAX navigation and logout handlers
- `template/profile-content.ctml` - Added AJAX navigation and logout handlers
- `template/front-page-content.ctml` - Added AJAX navigation and logout handlers
- `template/player-content.ctml` - Added AJAX navigation and logout handlers
- `template/status-content.ctml` - Created placeholder status page

## Testing Results
✅ Stream continues playing when clicking Login link  
✅ Stream continues playing during login form submission  
✅ Stream continues playing after successful login  
✅ Stream continues playing when clicking Logout  
✅ Stream continues playing when navigating between all pages (Home, Player, Status, Profile, Admin)  
✅ Login form works correctly with AJAX submission  
✅ Logout updates UI to show logged-out state  
✅ All navigation links maintain persistent audio  

## Additional Improvements
While fixing this issue, we also resolved:
1. **Now Playing panel not updating** - Scripts now execute after AJAX navigation
2. **MUTED indicator not appearing** - Spectrum analyzer properly re-initializes after navigation
3. **Console errors from abandoned intervals** - All intervals/timeouts are cleared before navigation
4. **Faster Now Playing updates** - Reduced initial update delay from 1s to 200ms

## Conclusion
The persistent player with frameset mode is now fully functional. Users can navigate throughout the entire application, including login/logout flows, without any interruption to the audio stream.
