;;; Commentary:

;; This package provides functions for manipulating colors, including
;; converting between color representations, computing color
;; complements, and computing CIEDE2000 color distances.
;;
;; Supported color representations include RGB (red, green, blue), HSV
;; (hue, saturation, value), HSL (hue, saturation, luminance), sRGB,
;; CIE XYZ, and CIE L*a*b* color components.

;;; Code:

;; Emacs < 23.3
(eval-and-compile
  (unless (boundp 'float-pi)
    (defconst float-pi (* 4 (atan 1)) "The value of Pi (3.1415926...).")))

(defun color-rgb-to-hex  (red green blue &optional digits-per-component)
  "Return hexadecimal #RGB notation for the color specified by RED GREEN BLUE.
RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive.
Optional argument DIGITS-PER-COMPONENT can be either 4 (the default)
or 2; use the latter if you need a 24-bit specification of a color."
  (or digits-per-component (setq digits-per-component 4))
  (let* ((maxval (if (= digits-per-component 2) 255 65535))
         (fmt (if (= digits-per-component 2) "#%02x%02x%02x" "#%04x%04x%04x")))
    (format fmt (* red maxval) (* green maxval) (* blue maxval))))

(defun color-complement (color-name)
  "Return the color that is the complement of COLOR-NAME.
COLOR-NAME should be a string naming a color (e.g. \"white\"), or
a string specifying a color's RGB
components (e.g. \"#ffff1212ecec\")."
  (let ((color (color-name-to-rgb color-name)))
    (list (- 1.0 (nth 0 color))
          (- 1.0 (nth 1 color))
          (- 1.0 (nth 2 color)))))
