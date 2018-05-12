;; superfluous chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun peach--install-if-necessary (theme-name version)
  "Install THEME-NAME is not already installed on the system."
  (let (
	(pkg (package-desc-create
	      :name (make-symbol (concat theme-name "-theme"))
	      :version (version-to-list version)
	      :kind 'single
	      :archive "melpa"
	      ))
	(screenshoter "gnome-screenshot"))
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun fetch-and-load-theme (theme-name version)
  "Get and install THEME-NAME before taking a screenshot of it."
  (peach--install-if-necessary theme-name version)
  (let ((cmd-name (concat "DISPLAY=:13 import -window root " default-directory "app/assets/images/" theme-name ".jpg")))
    (load-theme (intern theme-name) t)
    (toggle-frame-fullscreen)
    (find-file (concat default-directory "lib/sample.js"))
    (redisplay t)
    (sleep-for 1)
    (shell-command cmd-name nil nil)
    (disable-theme theme-name)
    (kill-emacs)))
