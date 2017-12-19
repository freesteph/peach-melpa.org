;; superfluous chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun peach--install-if-necessary (theme-name)
  "Install THEME-NAME is not already installed on the system."
  (let
      ((pkg-name (make-symbol (concat theme-name "-theme")))
       (screenshoter "gnome-screenshot"))
    (message "going to install %s" theme-name)
    (unless (package-installed-p pkg-name)
      (package-install pkg-name))))

(defun fetch-and-load-theme (theme-name)
  "Get and install THEME-NAME before taking a screenshot of it."
  (let ((cmd-name (concat "gnome-screenshot -B -w -f " default-directory "app/assets/images/" theme-name ".jpg")))
    (load-theme (intern theme-name) t)
    (find-file (concat default-directory "lib/sample.js"))
    (redisplay t)
    (sleep-for 1)
    (shell-command cmd-name nil nil)
    (disable-theme theme-name)))
