;; superfluous chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun peach--get-screenshot-cmd ()
  "Use the environment to figure out the screenshot command."
  (let ((peach-env (getenv "PEACH_ENV")))
    (cond ((string-equal "OSX" peach-env) "screencapture -C -o -t png ")
          ((string-equal "GNOME" peach-env) "gnome-screenshot -B -w -f ")
          ((string-equal "AWS" peach-env) "import -window root "))))

(defun peach--install-if-necessary (theme-name version)
  "Install THEME-NAME at VERSION revision if not already installed on the system."
  (let ((pkg (package-desc-create
              :name (make-symbol (concat theme-name "-theme"))
              :version (version-to-list version)
              :kind 'single
              :archive "melpa"
              )))
  (unless (package-installed-p pkg)
    (package-install pkg))))

(defun fetch-and-load-theme (theme-name version)
  "Get and install THEME-NAME at VERSION before taking a screenshot of it."
  (peach--install-if-necessary theme-name version)
  (let* (
         (screenshot-path (concat default-directory "app/assets/images/" theme-name ".jpg"))
         (cmd-name (concat (peach--get-screenshot-cmd) screenshot-path)))
    (load-theme (intern theme-name) t)
    (toggle-frame-fullscreen)
    (find-file (concat default-directory "lib/sample.js"))
    (redisplay t)
    (sleep-for 1)
    (shell-command cmd-name nil nil)
    (disable-theme theme-name)
    (kill-emacs)))

(provide 'take-screenshot)
;;; take-screenshot.el ends here
