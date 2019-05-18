;;; packages.el --- exwm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq exwm-packages
      '(cl-generic
        ;; (xelb :location (recipe :fetcher github
        ;;                         :repo "ch11ng/xelb")
        ;;       :step pre)
        ;; (exwm :location (recipe :fetcher github
        ;;                         :repo "ch11ng/exwm")
        ;;       :step pre)
        (xelb :location elpa)
        (exwm :location elpa)
        symon-lingr
        switch-window
        ))

(defun exwm/init-cl-generic ()
  (use-package cl-generic
    :demand))

(defun exwm/init-xelb ()
  (use-package xelb))

(defun exwm/init-switch-window ()
  (use-package switch-window
    :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-multiple-frames t)
    (setq switch-window-shortcut-style 'qwerty)))

(defun exwm/init-symon-lingr ()
  (use-package symon-lingr
    :config
    (setq symon-monitors
          '(symon-linux-memory-monitor
            symon-linux-cpu-monitor
            symon-linux-network-rx-monitor
            symon-linux-network-tx-monitor
            symon-linux-battery-monitor))
    (setq symon-delay 5)))

(defun exwm/init-exwm ()
  (use-package exwm
    :init
    ;; Disable dialog boxes since they are unusable in EXWM
    (setq use-dialog-box nil)
    ;; 5 Worskpaces please
    (setq exwm-workspace-number 5)
    ;; You may want Emacs to show you the time
    (display-time-mode t)
    (when exwm--hide-tiling-modeline
      (add-hook 'exwm-mode-hook #'hidden-mode-line-mode))
    (setq exwm-input-line-mode-passthrough t)
    ;; Show buffers from all workspaces/frames
    (setq exwm-workspace-show-all-buffers 't)
    (setq exwm-layout-show-all-buffers 't)

    ;; Focus follows mouse:
    ;;(setq mouse-autoselect-window t)
    ;;(setq focus-follows-mouse t)


    ;; The following example demonstrates how to use simulation keys to mimic the
    ;; Trying to make shell-pop with a real terminal :P
    ;; (defun exwm-launch-term ()
    ;;   (start-process-shell-command exwm--terminal-command
    ;;                                nil exwm--terminal-command))
    ;; (defun shell-pop-exwm-term (index)
    ;;   (interactive "P")
    ;;   (require 'shell-pop)
    ;;   (shell-pop--set-shell-type
    ;;    'shell-pop-shell-type
    ;;    '("exwm-term"
    ;;      "Termite" #'exwm-launch-term))
    ;;   (shell-pop index))
    :config
    ;; (when dotspacemacs-use-ido
    ;;   (exwm-enable-ido-workaround))

    ;; make sure that displaying transient states gets the keyboard input.  Also
    ;; take care of going into line mode, and possibly switching back.
    ;; borrowed from: https://github.com/abo-abo/hydra/issues/232
    (define-advice hydra-set-transient-map (:around (fun keymap on-exit &optional foreign-keys) exwm-passthrough)
      (spacemacs//exwm-switch-to-line-mode)
      (let ((on-exit (lexical-let ((on-exit on-exit))
                       (lambda ()
                         ;; Here would be the place to reactivate input state if
                         ;; it was active before hydra invocation.  This
                         ;; probably only makes sense when you have a global
                         ;; input state.
                         (when on-exit (funcall on-exit))))))
        (funcall fun keymap on-exit foreign-keys)))

    ;; override persp-mode's idea of frame creation for floating frames.  These
    ;; are characterized by the 'unsplittable' frame parameter, and should not
    ;; be tried to assign an existing layout to.

    (eval-after-load 'persp-mode
      (advice-add 'persp-init-new-frame :before-until 'spacemacs//exwm-persp-mode-inhibit-p))

    (add-hook 'exwm-update-class-hook 'setzerOS/exwm-auto-rename-buffer)
    (add-hook 'exwm-update-title-hook 'setzerOS/exwm-auto-rename-buffer)

    ;; kick all exwm buffers into insert mode per default
    (add-hook 'exwm-manage-finish-hook (lambda () (call-interactively #'exwm-input-release-keyboard)))

    (defvar exwm-workspace-switch-wrap t
      "Whether `spacemacs/exwm-workspace-next' and `spacemacs/exwm-workspace-prev' should wrap.")

    ;; Quick swtiching between workspaces
    (defvar exwm-toggle-workspace 0
      "Previously selected workspace. Used with `spacemacs/exwm-jump-to-last-exwm'.")

    (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
      (setq exwm-toggle-workspace exwm-workspace-current-index))

    ;; Startup programs
    (add-hook 'exwm-init-hook
              (lambda ()
                (spacemacs/exwm-run-program-in-home
                 "compton --config ~/.compton.conf")))
    (add-hook 'exwm-init-hook (lambda () (symon-mode)))

    ;; `exwm-input-set-key' allows you to set a global key binding (available in
    ;; any case). Following are a few examples.
    ;; + We always need a way to go back to line-mode from char-mode
    (exwm-input-set-key (kbd "s-<escape>") 'exwm-reset)

    (exwm-input-set-key (kbd "s-f") #'spacemacs/exwm-layout-toggle-fullscreen)
    (exwm-input-set-key (kbd "<s-tab>") #'spacemacs/exwm-jump-to-last-exwm)
    ;; + Bind a key to switch workspace interactively
    (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
    ;; + Set shortcuts to switch to a certain workspace.
    (exwm-input-set-key (kbd "s-1")
                        (lambda () (interactive) (exwm-workspace-switch 0)))
    (exwm-input-set-key (kbd "s-2")
                        (lambda () (interactive) (exwm-workspace-switch 1)))
    (exwm-input-set-key (kbd "s-3")
                        (lambda () (interactive) (exwm-workspace-switch 2)))
    (exwm-input-set-key (kbd "s-4")
                        (lambda () (interactive) (exwm-workspace-switch 3)))
    (exwm-input-set-key (kbd "s-5")
                        (lambda () (interactive) (exwm-workspace-switch 4)))
    (exwm-input-set-key (kbd "s-6")
                        (lambda () (interactive) (exwm-workspace-switch 5)))
    (exwm-input-set-key (kbd "s-7")
                        (lambda () (interactive) (exwm-workspace-switch 6)))
    (exwm-input-set-key (kbd "s-8")
                        (lambda () (interactive) (exwm-workspace-switch 7)))
    (exwm-input-set-key (kbd "s-9")
                        (lambda () (interactive) (exwm-workspace-switch 8)))
    (exwm-input-set-key (kbd "s-0")
                        (lambda () (interactive) (exwm-workspace-switch 9)))
    ;; + Application launcher ('M-&' also works if the output buffer does not
    ;;   bother you). Note that there is no need for processes to be created by
    ;;   Emacs.
    (exwm-input-set-key (kbd "s-r") #'spacemacs/exwm-app-launcher)
    ;; + 'slock' is a simple X display locker provided by suckless tools. 'i3lock'
    ;;   is a more feature-rich alternative.
    (exwm-input-set-key (kbd "<s-pause>")
                        (lambda () (interactive) (start-process-shell-command "lock" nil exwm--locking-command)))

    ;; Program Launchers
    (exwm-input-set-key (kbd "s-I")
                        (lambda () (interactive)
                          (setzerOS/find-or-create "^Firefox/.*$" "firefox")))
    (exwm-input-set-key (kbd "s-O")
                        (lambda () (interactive)
                          (setzerOS/find-or-create "^Dolphin/.*$"
                                                   "export $(dbus-launch) && dolphin")))
    (exwm-input-set-key (kbd "s-T")
                        (lambda () (interactive)
                          (setzerOS/find-or-create "^TelegramDesktop/.*$"
                                                   "telegram-desktop")))
    (exwm-input-set-key (kbd "s-M") 
                        (lambda () (interactive)
                          (setzerOS/find-or-create "^Thunderbird/.*$" "thunderbird")))
    (exwm-input-set-key (kbd "<s-return>")
                        (lambda () (interactive)
                          (setzerOS/find-or-create "^qterminal/.*$" "qterminal")))
    (exwm-input-set-key (kbd "<S-s-return>")
                        (lambda () (interactive)
                          (spacemacs/exwm-run-program-in-home "qterminal")))
    (exwm-input-set-key (kbd "s-B") 'setzerOS/helm-application-launcher)

    (exwm-input-set-key (kbd "s-c") 'calc)

    ;; WM commands
    (exwm-input-set-key (kbd "s-w") 'delete-other-windows)
    (exwm-input-set-key (kbd "s-h") #'setzerOS/focus-window-left)
    (exwm-input-set-key (kbd "s-j") #'setzerOS/focus-window-down)
    (exwm-input-set-key (kbd "s-k") #'setzerOS/focus-window-up)
    (exwm-input-set-key (kbd "s-l") #'setzerOS/focus-window-right)

    ;; Evil-ex
    (exwm-input-set-key (kbd "s-:") #'evil-ex)

    ;; ensure that when char mode is left, state is restored to normal
    (advice-add 'exwm-input-grab-keyboard :after (lambda (&optional id)
                                                   (evil-normal-state)))
    ;; ensure that when char mode is entered, input state is activated
    (advice-add 'exwm-input-release-keyboard :after (lambda(&optional id)
                                                      (evil-insert-state)))

    ;; TODO: optionally inhibit switching to char mode or line mode, used during transient state

    ;; in normal state/line mode, use the familiar i key to switch to input state
    (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
    (push ?\i exwm-input-prefix-keys)

    ;; regular space leader keys in line mode
    (defun spacemacs//exwm-convert-key-to-event (key)
      "Converts something from (kbd ...) format to something suitable for
    exwm-input-prefix-keys"
      (let ((key (kbd key)))
        (if (and (sequencep key)
                 (= (length key) 1))
            (etypecase key
              (string (string-to-char key))
              (vector (elt key 0)))
          (error "cannot convert to key event: %s" key))))

    ;; (push ?\  exwm-input-prefix-keys)
    (push (spacemacs//exwm-convert-key-to-event dotspacemacs-leader-key) exwm-input-prefix-keys)
    (push (spacemacs//exwm-convert-key-to-event dotspacemacs-emacs-leader-key) exwm-input-prefix-keys)
    ;; introduce new universal leader: s-SPC
    ;; buggy:
    (exwm-input-set-key (kbd "s-SPC") spacemacs-default-map)

    ;; Use s-Q to close buffers
    (exwm-input-set-key (kbd "s-Q") 'spacemacs/kill-this-buffer)
    ;; Use s-q to close windows 
    (exwm-input-set-key (kbd "s-q") 'setzerOS/delete-window-unless-last)

    ;; Universal Get-me-outta-here
    (push ?\C-g exwm-input-prefix-keys)
    ;; Universal Arguments
    (push ?\C-u exwm-input-prefix-keys)
    (push ?\C-0 exwm-input-prefix-keys)
    (push ?\C-1 exwm-input-prefix-keys)
    (push ?\C-2 exwm-input-prefix-keys)
    (push ?\C-3 exwm-input-prefix-keys)
    (push ?\C-4 exwm-input-prefix-keys)
    (push ?\C-5 exwm-input-prefix-keys)
    (push ?\C-6 exwm-input-prefix-keys)
    (push ?\C-7 exwm-input-prefix-keys)
    (push ?\C-8 exwm-input-prefix-keys)
    (push ?\C-9 exwm-input-prefix-keys)
    ;; C-c, C-x are needed for copying and pasting
    (delete ?\C-x exwm-input-prefix-keys)
    (delete ?\C-c exwm-input-prefix-keys)
    ;; We can use `M-m h' to access help
    (delete ?\C-h exwm-input-prefix-keys)

    ;; introduce leader for running programs
    (spacemacs/declare-prefix "&" "exwm-run")
    (spacemacs/set-leader-keys "&s" 'spacemacs/exwm-launch-split-below)
    (spacemacs/set-leader-keys "&v" 'spacemacs/exwm-launch-split-right)

    ;; Preserve the habit
    ;; (exwm-input-set-key (kbd "s-:") 'helm-M-x)
    ;; (exwm-input-set-key (kbd "s-;") 'evil-ex)
    ;; Shell (not a real one for the moment)
    ;; (exwm-input-set-key (kbd "C-'") #'spacemacs/default-pop-shell)
    ;; Undo window configurations
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "s-U") #'winner-redo)
    ;; Change buffers
    (exwm-input-set-key (kbd "s-b") #'ivy-switch-buffer)
    ;; Moving Windows
    (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
    (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
    (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
    (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
    ;; Resize
    (exwm-input-set-key (kbd "M-s-h") #'spacemacs/shrink-window-horizontally)
    (exwm-input-set-key (kbd "M-s-j") #'spacemacs/enlarge-window)
    (exwm-input-set-key (kbd "M-s-k") #'spacemacs/shrink-window)
    (exwm-input-set-key (kbd "M-s-l") #'spacemacs/enlarge-window-horizontally)
    (exwm-input-set-key (kbd "s-m") #'spacemacs/toggle-maximize-buffer)
    ;; Workspaces
    (exwm-input-set-key (kbd "s-]") #'spacemacs/exwm-workspace-next)
    (exwm-input-set-key (kbd "s-[") #'spacemacs/exwm-workspace-prev)

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
          (-interleave (number-sequence 0 9)
                       (-flatten (-repeat 5 '("eDP1" "DP1")))))
    (exwm-randr-enable)

    ;; Per-window-class settings
    (setq exwm-manage-configurations
          '(((equal exwm-class-name "plasmashell")
             floating nil)))
    

    ;; Custom Modeline (it's almost equal to spacemacs modeline)
    ;; (require 'spaceline)
    ;; (with-eval-after-load "spaceline"
    ;;   (spaceline-compile
    ;;     ;; left side
    ;;     '(((persp-name
    ;;         workspace-number
    ;;         window-number)
    ;;        :fallback evil-state
    ;;        :face highlight-face
    ;;        :priority 100)
    ;;       (exwm-buttons :when (and active (eq major-mode 'exwm-mode)))
    ;;       (anzu :priority 95)
    ;;       auto-compile
    ;;       ((buffer-modified buffer-size buffer-id remote-host)
    ;;        :priority 98)
    ;;       (major-mode :priority 79)
    ;;       (process :when active)
    ;;       ((flycheck-error flycheck-warning flycheck-info)
    ;;        :when active
    ;;        :priority 89)
    ;;       (minor-modes :when active
    ;;                    :priority 9)
    ;;       (mu4e-alert-segment :when active)
    ;;       (erc-track :when active)
    ;;       (version-control :when active
    ;;                        :priority 78)
    ;;       (org-pomodoro :when active)
    ;;       (org-clock :when active)
    ;;       nyan-cat)
    ;;     ;; right side
    ;;     '(which-function
    ;;       (python-pyvenv :fallback python-pyenv)
    ;;       (purpose :priority 94)
    ;;       (battery :when active)
    ;;       (selection-info :priority 95)
    ;;       input-method
    ;;       ((buffer-encoding-abbrev
    ;;         point-position
    ;;         line-column)
    ;;        :separator " | "
    ;;        :priority 96)
    ;;       (global :when active)
    ;;       (buffer-position :priority 99)
    ;;       (hud :priority 99))))
    ))
