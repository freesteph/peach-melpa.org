;; superfluous chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

; default font
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :weight 'light
                    :width 'normal
                    :height 180)

(setq org-startup-folded nil)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun peach--get-screenshot-cmd ()
  "Use the environment to figure out the screenshot command."
  (let ((peach-env (getenv "PEACH_ENV")))
    (if (string-equal "OSX" peach-env)
        "screencapture -C -o -t png "
          "import -window root ")))

(defun peach--install-if-necessary (theme-name version kind)
  "Install THEME-NAME of type KIND at VERSION revision if not already installed on the system."
  (let ((pkg (package-desc-create
              :name (make-symbol (concat theme-name "-theme"))
              :version (version-to-list version)
              :kind (intern kind)
              :archive "melpa"
              )))
  (unless (package-installed-p pkg)
    (package-install pkg))))

(defun peach--capture-screenshot-for-mode (theme-name mode)
  "Find the correct MODE sample for THEME-NAME of package type KIND and screenshot it."
  (let* ((screenshot-path (format "%stmp/screenshots/%s_%s.png" default-directory theme-name mode))
         (sample-path (format "%slib/samples/*.%s" default-directory mode))
         (cmd-name (concat (peach--get-screenshot-cmd) screenshot-path)))
    (find-file sample-path t)
    (cd "../../")
    (redisplay t)
    (sleep-for 1)
    (shell-command cmd-name nil nil)))

(defun fetch-and-load-theme (theme-name version kind)
  "Get and install THEME-NAME of package type TYPE and VERSION before taking a screenshot of it."
  (peach--install-if-necessary theme-name version kind)

  (load-theme (intern theme-name) t)
  (setq frame-resize-pixelwise t)
  (toggle-frame-fullscreen)

  (let ((modes '(el js c rb org)))
    (while modes
      (setq mode (car modes))
      (peach--capture-screenshot-for-mode theme-name mode)
      (setq modes (cdr modes))))

  (disable-theme theme-name)
  (kill-emacs 0))


(provide 'take-screenshot)
;;; take-screenshot.el ends here
