(provide 'fringe-center-code)

(defun center-text-horizontally ()
  "Set fringe style based on mode, number of windows in frame, and fullscreen."
  (interactive)
  (if (= (length (window-list)) 1)
    (set-fringe-style '(500 . 500))
    (set-fringe-style '(nil . nil))))

(defadvice mac-toggle-max-window (after mac-toggle-max-window-adjust-fringe
                                        activate)
  "Depending on fullscreen mode and number of windows splitting frame, switch fringe style"
  (center-text-horizontally))

(defadvice switch-to-buffer (after switch-to-buffer-adjust-fringe activate)
  "Depending on major mode, switch fringe style"
  (center-text-horizontally))

(defadvice delete-other-windows (after delete-other-windows-adjust-fringe
                                       activate)
  (center-text-horizontally))

(defadvice delete-window (after delete-window-adjust-fringe activate)
  (center-text-horizontally))

(defadvice split-window (after split-window-adjust-fringe activate)
  (center-text-horizontally))

(defadvice ido-kill-buffer (after ido-kill-buffer-adjust-fringe activate)
  (center-text-horizontally))

(defadvice with-output-to-temp-buffer
  (after with-output-to-temp-buffer-adjust-fringe activate)
  (center-text-horizontally))

(defadvice delete-completion-window
  (after delete-completion-window-fringe activate)
  (center-text-horizontally))

(defadvice kill-buffer
  (after kill-buffer-fringe activate)
  (center-text-horizontally))

(defadvice kill-buffer-and-window
  (after kill-buffer-and-window-fringe activate)
  (center-text-horizontally))
