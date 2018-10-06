(require 'cl)
;; superfluous chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

default font
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :weight 'light
                    :width 'normal
                    :height 180)

(setq org-startup-folded nil)
(setq frame-resize-pixelwise t)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-refresh-contents)

(defun peach--get-screenshot-cmd ()
  "Use the environment to figure out the screenshot command."
  (let ((peach-env (getenv "PEACH_ENV")))
    (if (string-equal "OSX" peach-env)
        "screencapture -C -o -t png "
          "import -window root ")))

(defun peach--ensure-clean-install (theme-name)
  "Ensure THEME-NAME of VERSION and KIND is removed before starting."
  (let ((pkg (assoc (intern theme-name) package-alist)))
    (and (not (null pkg))
	 (package-delete (cadr pkg)))))

(defun peach--install (theme-name)
  "Installs the theme designed by THEME-NAME."
  (let ((desc (cadr (assoc (intern theme-name) package-archive-contents))))
    (and (null desc) (error "The theme is not available"))
    (package-install desc)))

(defun peach--capture-screenshot-for-mode (theme-name mode)
  "Find the correct MODE sample for THEME-NAME of package type KIND and screenshot it."
  (let* ((screenshot-path (format "%stmp/screenshots/%s_%s.png" default-directory theme-name mode))
         (sample-path (format "%slib/samples/*.%s" default-directory mode))
         (cmd-name (concat (peach--get-screenshot-cmd) screenshot-path)))
    (save-excursion
      (find-file sample-path t)
      (redisplay t)
      (shell-command cmd-name nil nil))))

(defun fetch-and-load-theme (theme-name version kind)
  "Get and install THEME-NAME of package type KIND and VERSION before taking a screenshot of it."
  (peach--ensure-clean-install theme-name)
  (setq current-themes (custom-available-themes))
  (peach--install theme-name)
  (setq possible-themes (set-difference (custom-available-themes) current-themes))

  (toggle-frame-fullscreen)

  (dolist
      (variant possible-themes)
    (condition-case nil
	(progn
	  (load-theme variant t)
	  (let ((modes '(el js c rb org)))
	    (while modes
	      (setq mode (car modes))
	      (peach--capture-screenshot-for-mode variant mode)
	      (setq modes (cdr modes))))
	  (disable-theme variant))
      (error nil))))

(provide 'take-screenshot)
;;; take-screenshot.el ends here
