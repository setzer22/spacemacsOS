;; Can be used to bind a key to jumping to an application, or alternatively starting it.  E.g.:
;;
;; (spacemacs/exwm-bind-switch-to-or-run-command "s-f" "Firefox" "firefox")
;;
;; The window class can be found out with exwm's builtin info functions, but for most applications it should just match the buffer name.
(defun spacemacs/exwm-bind-switch-to-or-run-command (key window-class command)
  (exwm-input-set-key (kbd key)
                      `(lambda ()
                         (interactive)
                         (spacemacs/exwm-switch-to-buffer-or-run ,window-class ,command))))

(defun spacemacs//exwm-switch-to-line-mode ()
  "Used as a hook to switch to line mode when transient mode starts."
  (when (eq exwm--input-mode 'char-mode)
    ;; (setq exwm--switch-to-char-after-transient (current-buffer))
    (call-interactively 'exwm-input--grab-keyboard)))

(defun spacemacs//exwm-persp-mode-inhibit-p (frame)
  (frame-parameter frame 'unsplittable))

(defun spacemacs/exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

(defun spacemacs/exwm-switch-to-buffer-or-run (window-class command)
  "Switch to first buffer with window-class, and if not present, run command."
  (let ((buffer
         (find window-class (buffer-list) :key (lambda(b) (cdr (assoc 'exwm-class-name (buffer-local-variables b)))) :test 'string-equal)))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (start-process-shell-command command nil command))))

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(defun spacemacs/exwm-rename-buffer ()
  (let* ((part1 exwm-class-name)
         (part2 (when (not (string-equal exwm-class-name exwm-title))
                  (concat "/" exwm-title)))
         (name (concat part1 (or part2 "")))
         (maxlen 40))
    (exwm-workspace-rename-buffer (if (> (length name) maxlen)
                                      (concat (subseq name 0 (- maxlen 3)) "...")
                                    name))))

(defun spacemacs/exwm-workspace-next ()
  "Switch to next exwm-workspaceective (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index
                       (1- exwm-workspace-number))))
    (cond
     (only-workspace? nil)
     (overflow?
      (when exwm-workspace-switch-wrap
        (exwm-workspace-switch 0)))
     (t (exwm-workspace-switch  (1+ exwm-workspace-current-index))))))

(defun spacemacs/exwm-workspace-prev ()
  "Switch to next exwm-workspaceective (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index 0)))
    (cond
     (only-workspace? nil)
     (overflow?
      (when exwm-workspace-switch-wrap
        (exwm-workspace-switch (1- exwm-workspace-number))))
     (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

(defun spacemacs/exwm-layout-toggle-fullscreen ()
  "Togggles full screen for Emacs and X windows"
  (interactive)
  (if (eq major-mode 'exwm-mode)
      (exwm-layout-toggle-fullscreen)
    (spacemacs/toggle-maximize-buffer)))

(defun spacemacs/exwm-run-program-in-home (command)
  (let ((default-directory user-home-directory))
    (start-process-shell-command command nil command)))

(defun spacemacs/exwm-app-launcher (command)
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ivy"
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-launch-split-below (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-below-and-focus)
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-launch-split-right (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-right-and-focus)
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm-toggle-workspace))

(defun spacemacs/exwm-exwm-buffers-info ()
  "Helper, return information about open exwm windows"
  (loop for buffer in (buffer-list)
        for name = (buffer-name buffer)
        for ecname = (buffer-local-value 'exwm-class-name buffer)
        when ecname
        collect (list :buffer-name name :exwm-class-name ecname)))

;; ===================================
;; | setzerOS standard lib functions |
;; ===================================

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defun -get (alist k)
  (cdr (assoc k alist)))

(defun slurp (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun elisp-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; ===========================
;; | setzerOS find-or-create |
;; ===========================

(defun find-buffers-matching-regexp (regexp)
  "Returns the list of buffers whose titles match `regexp'"
  (-filter
   (lambda (buffer)
     (string-match-p regexp (buffer-name buffer)))
   (buffer-list)))

(defun setzerOS/find-or-create (buffer-regexp command)
  "This function can be used as a helper function for hotkeys.
`buffer-regexp' is a regexp that will try to match a buffer
title. If found, the buffer will be brought to the currently
selected window. Otherwise, `command' will be run.

Examples:

   (find-or-create \"^Firefox/.*$\" \"firefox\")
   (find-or-create \"^Thunderbird/.*$\" \"thunderbird\")"

  (let ((buffers (find-buffers-matching-regexp buffer-regexp)))
    (if buffers
        (set-window-buffer (selected-window)
                           (first buffers))
      (spacemacs/exwm-run-program-in-home command))))

;; =================================
;; | setzerOS application-launcher |
;; =================================

(defvar setzerOS/desktop-file-paths
  '("/usr/share/applications"
    "~/.local/share/applications"))

(defun setzerOS/get-all-desktop-files ()
  "Returns a list of all .desktop files in setzerOS/desktop-file-paths"
  (let ((paths setzerOS/desktop-file-paths))
    (-filter
     (lambda (x) (string-match-p "^.*\.desktop$" x))
     (-mapcat
      (lambda (x) (directory-files x 't))
      paths))))

(defun setzerOS/parse-desktop-file (path)
  "Parses a .desktop file into an alist."
  (let* ((d (read-lines path))
         (attrs-alist
          (->> d
               (-map (lambda (x) (split-string x "=")))
               (-filter (lambda (x) (= (length x) 2)))
               (-map (lambda (x) (cons (first x) (second x)))))))
    attrs-alist))

(defvar setzerOS/desktop-launchers
  nil
  "The cached list of parsed desktop files to fill
`setzerOS/helm-application-launcher'. Set by
`setzerOS/refresh-desktop-launchers'")

(defun setzerOS/refresh-desktop-launchers ()
  "Fills `setzerOS/desktop-launchers'."
  (setq setzerOS/desktop-launchers
        (-map (lambda (x)
                (let ((parsed (setzerOS/parse-desktop-file x)))
                  (-insert-at 0 (cons 'path x) parsed)))
              (setzerOS/get-all-desktop-files))))

(defun setzerOS/helm-application-launcher ()
  "Runs an interactive application launcher using `helm'. The launcher will
display two sources: A list of open buffers and a list of applications (from
`setzerOS/desktop-launchers') to run."
  (interactive)
  (when (eq setzerOS/desktop-launchers nil)
    (setzerOS/refresh-desktop-launchers))
  (let*
      ((parsed-desktop-files setzerOS/desktop-launchers)
       (source1 (helm-build-sync-source "Launch Application"
                  :candidates
                  (-map
                   (lambda (parsed)
                     (concat (-get parsed "Name") " -- " (-get parsed "Comment")
                             "  {" (file-name-nondirectory
                                    (file-name-sans-extension (-get parsed 'path))) "}"))
                   parsed-desktop-files)
                  :action
                  '(("Run" . (lambda (x)
                               (let* ((appname (progn
                                                 (string-match ".*{\\(.*\\)}$" x)
                                                 (match-string 1 x))))
                                 (start-process-shell-command
                                  appname nil
                                  (format "/bin/bash -c 'setsid gtk-launch %s'" appname)))))))))
    (helm
     :buffer "*helm application launcher*"
     :sources '(helm-source-buffers-list source1))))

;; ================================
;; | setzerOS multi-monitor focus |
;; ================================

(defun get-frame-position (f)
  (let ((positions (-map 'window-absolute-pixel-edges (window-list f) )))
    (list
     (seq-min (-map 'first positions))
     (seq-min (-map 'second positions)))))

(defun --frame-dir-impl (f coord-sel cmp)
  (let* ((f-x (funcall coord-sel (get-frame-position f)))
         (frames (frame-list-z-order))
         (frame-xs (-map (lambda (f) (funcall coord-sel (get-frame-position f)))
                         frames))
         (frames-right (->>
                        (-zip frames frame-xs)
                        (-filter (lambda (fr-fr-x)
                                   (let ((x (cdr fr-fr-x)))
                                     (funcall cmp x f-x)))))))
    (when (> (length frames-right) 0)
      (let* ((closest-frame-x (seq-min (-map 'cdr frames-right)))
             (frame-right (car
                           (first
                            (-drop-while (lambda (x) (not (= (cdr x) closest-frame-x)))
                                         frames-right)))))
        frame-right))))

(defun frame-in-direction (f dir)
  "Returns the frame situated at direction 'dir' from 'f'"
  (cond
   ((eq dir 'right) (--frame-dir-impl f 'car '>))
   ((eq dir 'left) (--frame-dir-impl f 'car '<))
   ((eq dir 'above) (--frame-dir-impl f 'cdr '<))
   ((eq dir 'below) (--frame-dir-impl f 'cdr '>))))

(defun --window-in-direction-multiframe-impl (w dir get-x get-y)
  (let ((w-r (window-in-direction dir w)))
    (if w-r w-r
      (let* ((f (frame-in-direction (window-frame w) dir)))
        (when f
          (let* ((edges (window-edges w))
                 (x (funcall get-x edges (frame-width f) (frame-height f)))
                 (y (funcall get-y edges (frame-width f) (frame-height f))))
            (window-at x y f)))))))

(defun window-in-direction-multiframe (dir w)
  "Like `window-in-direction', but works for multiple frames."
  (cond
   ((eq dir 'right) (--window-in-direction-multiframe-impl
                     w 'right
                     (lambda (edges w h) 1)
                     (lambda (edges w h) (/ (+ (nth 1 edges) (nth 3 edges)) 2))))
   ((eq dir 'left)  (--window-in-direction-multiframe-impl
                     w 'left
                     (lambda (edges w h) w)
                     (lambda (edges w h) (/ (+ (nth 1 edges) (nth 3 edges)) 2))))
   ((eq dir 'above) (--window-in-direction-multiframe-impl
                     w 'above
                     (lambda (edges w h) (/ (+ (nth 0 edges) (nth 2 edges)) 2))
                     (lambda (edges w h) h)))
   ((eq dir 'below) (--window-in-direction-multiframe-impl
                     w 'below
                     (lambda (edges w h) (/ (+ (nth 0 edges) (nth 2 edges)) 2))
                     (lambda (edges w h) 1)))))

(defun setzerOS/focus-window-in-direction (dir)
  "Focuses window in given direction. Can focus across multiple frames"
  (let ((w (window-in-direction-multiframe dir (selected-window))))
    (if w
        (let* ((edges (window-edges w))
               (midpoint-x (/ (+ (nth 0 edges) (nth 2 edges)) 2))
               (midpoint-y (/ (+ (nth 1 edges) (nth 3 edges)) 2)))
          (select-window w)
          (set-mouse-position (window-frame w) midpoint-x midpoint-y))
      (print (format "No window %s of current selection." dir)))))

(defun setzerOS/focus-window-left ()
  "Focuses window to the left. Can move across multiple frames"
  (interactive)
  (setzerOS/focus-window-in-direction 'left))

(defun setzerOS/focus-window-right ()
  "Focuses window to the right. Can move across multiple frames"
  (interactive)
  (setzerOS/focus-window-in-direction 'right))

(defun setzerOS/focus-window-up ()
  "Focuses window up. Can move across multiple frames"
  (interactive)
  (setzerOS/focus-window-in-direction 'above))

(defun setzerOS/focus-window-down ()
  "Focuses window down. Can move across multiple frames"
  (interactive)
  (setzerOS/focus-window-in-direction 'below))

;; ===================
;; | Buffer Renaming |
;; ===================

(defvar setzerOS/buffers-to-avoid-rename
  '())

(defun setzerOS/exwm-auto-rename-buffer ()
  "A wrapper for `spacemacs/exwm-rename-buffer', which avoids renaming buffers inside
the list `setzerOS/buffers-to-avoid-rename'"
  (when (not (-contains? setzerOS/buffers-to-avoid-rename
                         (current-buffer)))
    (spacemacs/exwm-rename-buffer)))

(defun setzerOS/exwm-rename-buffer ()
  "A wrapper for `rename-buffer' that puts the renamed buffer
inside `setzerOS/buffers-to-avoid-rename'."
  (interactive)
  (let ((new-name (read-string "New buffer name: "))
        (buffer (current-buffer)))
    (rename-buffer new-name)
    (add-to-list 'setzerOS/buffers-to-avoid-rename buffer)))

;; ===================
;; | setzerOS splits |
;; ===================

(defun setzerOS/latex-split ()
  (interactive)
  (split-window nil
                (truncate (- (* 0.4 (window-total-width)))) 'right)
  (let ((w (window-right (selected-window))))
    (split-window w
                  (truncate (- (* 0.3 (window-total-height w)))) 'below)))


;; ======================================
;; | setzerOS delete window unless last |
;; ======================================

(defun setzerOS/delete-window-unless-last ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-window)
    (print "Cannot delete last window in frame")))

;; ========================================
;; | setzerOS get buffer from run command |
;; ========================================

(defun --run-command-buffer-callback-callback (title-regexp bufs-before callback)
  "Implementation function (because recurisve lambdas that recurse
inside a timer in elisp are tricky). See `setzerOS/run-command-buffer-callback'"
  (let* ((bufs-after (buffer-list))
         (diff (-difference bufs-after bufs-before))
         (matches (-filter
                   (lambda (x)
                     (string-match-p title-regexp (buffer-name x)))
                   diff))
         (match (first matches)))
    (if match
        (funcall callback match)
      (run-at-time 0.5 nil #'--run-command-buffer-callback-callback title-regexp bufs-after callback))))

(defun run-command-buffer-callback (command title-regexp callback)
  "Runs `command' as a program in your home directory. Waits for a buffer matching
`title-regexp' to be generated by `command' (although this is not checked) and
executes the `callback' lambda, which takes the newly created buffer as parameter.

Example:

(run-command-buffer-callback \"firefox\" \"^Firefox/.*$\" (lambda (firefox-buf) (print (buffer-name firefox-buf))))

will print \"Firefox/Mozilla Firefox\" to the *Messages* buffer after firefox is launched."
  (spacemacs/exwm-run-program-in-home command)
  (cl-labels ()
    (let ((bufs-before (buffer-list)))
      (run-at-time 0.5 nil
                   #'--run-command-buffer-callback-callback
                   title-regexp
                   bufs-before
                   callback))))

;; ================================================================
;; | setzerOS Utility functions using run-command-buffer-callback |
;; ================================================================

;; ============================
;; | setzerOS Modeline tweaks |
;; ============================

(defpowerline powerline-close-window
  (propertize "⨉"
              'mouse-face 'mode-line-highlight
              'help-echo "Close this window"
              'local-map (let ((map (make-sparse-keymap)))
			                     (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
			                     map)))

(defpowerline powerline-toggle-float
  (propertize "⬜"
              'mouse-face 'mode-line-highlight
              'help-echo "Toggle floating state"
              'local-map (let ((map (make-sparse-keymap)))
			                     (define-key map [mode-line mouse-1] 'exwm-floating-toggle-floating)
			                     map)))

(spaceline-define-segment exwm-buttons
  "Docstring"
  (list
   (powerline-toggle-float)
   (powerline-close-window))
  :priority 110
  :enabled 't)
