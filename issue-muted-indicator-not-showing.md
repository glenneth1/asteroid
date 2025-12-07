# Issue: MUTED Indicator Not Appearing in Spectrum Analyzer After Navigation

## Description
The "MUTED" notification in the spectrum analyzer does not appear after navigating between pages using AJAX in frameset mode. The indicator shows correctly on initial page load, but after navigating to another page and back, muting the audio stream does not display the "MUTED" text overlay on the spectrum analyzer canvas.

## Steps to Reproduce
1. Navigate to `/asteroid/frameset`
2. Mute the audio stream - "MUTED" indicator appears correctly ✓
3. Click "Player" to navigate to the player page
4. Mute the audio stream - "MUTED" indicator does NOT appear ✗
5. Navigate back to the home page
6. Mute the audio stream - "MUTED" indicator does NOT appear ✗

## Expected Behavior
The "MUTED" indicator should:
- Display prominently on the spectrum analyzer canvas when audio is muted
- Work consistently across all pages with a spectrum analyzer
- Continue working after AJAX navigation
- Update in real-time when mute state changes

## Root Cause
Multiple issues are causing this problem:

### 1. Stale Audio Element Reference
The spectrum analyzer stores a reference to the audio element in `*current-audio-element*` during initialization. After AJAX navigation:
- The DOM is replaced with new content
- The old audio element reference becomes stale
- The spectrum analyzer is still checking the muted property of the old (no longer valid) element
- The animation loop continues running but with outdated references

### 2. Frame Access Method
In frameset mode, the audio element lives in the `player-frame` while the spectrum analyzer canvas is in the `content-frame`. The code is using `window.parent.frames.namedItem()` which is not a standard method and causes errors:
```
TypeError: window.parent.frames.namedItem is not a function
```

### 3. No Re-initialization After Navigation
When content is loaded via AJAX, the spectrum analyzer initialization function runs but:
- The animation loop is already running from the previous page
- The new audio element reference is stored but the running loop doesn't pick it up
- The analyzer needs to be stopped and restarted with the fresh reference

## Proposed Solution

### 1. Fix Frame Access
Change from `namedItem()` to standard property access:
```lisp
;; Before (broken):
(let ((player-frame (ps:chain (ps:@ window parent frames) (named-item "player-frame"))))

;; After (working):
(let ((player-frame (ps:getprop (ps:@ window parent frames) "player-frame")))
```

### 2. Proper Initialization Order
Update the audio element lookup to check the current document first, then fall back to parent frame:
```lisp
;; Try current document first
(setf audio-element (or (ps:chain document (get-element-by-id "live-audio"))
                        (ps:chain document (get-element-by-id "persistent-audio"))))

;; If not found and we're in a frame, try parent frame (frameset mode)
(when (and (not audio-element)
           (ps:@ window parent)
           (not (eq (ps:@ window parent) window)))
  (ps:try
   (let ((player-frame (ps:getprop (ps:@ window parent frames) "player-frame")))
     (when player-frame
       (setf audio-element (ps:chain player-frame document (get-element-by-id "persistent-audio")))))))
```

### 3. Restart Analyzer After Re-initialization
Modify `initialize-spectrum-analyzer()` to:
1. Stop the existing animation loop
2. Update the audio element reference
3. Restart the analyzer if audio is already playing

```lisp
(defun initialize-spectrum-analyzer ()
  ;; Stop existing analyzer if running
  (when *animation-id*
    (ps:chain window (cancel-animation-frame *animation-id*))
    (setf *animation-id* nil))
  
  ;; ... find and store audio element ...
  
  ;; If audio is already playing, restart the analyzer with new reference
  (when (and (not (ps:@ audio-element paused))
             (ps:chain document (get-element-by-id "spectrum-canvas")))
    (init-spectrum-analyzer)))
```

### 4. Call Re-initialization After AJAX Navigation
Add call to `initializeSpectrumAnalyzer()` in the `loadInFrame()` function:
```javascript
// Re-initialize spectrum analyzer after navigation
if (window.initializeSpectrumAnalyzer) {
  setTimeout(() => window.initializeSpectrumAnalyzer(), 100);
}
```

## Files to Modify
- `parenscript/spectrum-analyzer.lisp` - Fix frame access, initialization order, and restart logic
- `template/front-page-content.ctml` - Add re-initialization call
- `template/player-content.ctml` - Add re-initialization call
- `template/admin-content.ctml` - Add re-initialization call
- `template/profile-content.ctml` - Add re-initialization call
- `template/login-content.ctml` - Add re-initialization call
- `template/status-content.ctml` - Add re-initialization call

## Testing Plan
After implementing the fix:
1. Navigate to `/asteroid/frameset`
2. Mute audio - verify "MUTED" appears
3. Navigate to player page
4. Mute audio - verify "MUTED" appears
5. Navigate to any other page
6. Mute audio - verify "MUTED" appears
7. Unmute and remute - verify indicator updates in real-time

## Additional Benefits
This fix will also ensure:
- Spectrum analyzer theme/style preferences persist across navigation
- Canvas border colors update correctly
- All spectrum analyzer features work consistently across pages
- Proper resource cleanup when switching pages
