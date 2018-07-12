;;; slim-modeline.el --- A package provides functionality to minimize mode line.

;; Copyright (C) 2018  kaiwk

;; Author: kaiwk <kaiwkx@gmail.com>
;; Version: 1.0
;; URL: https://github.com/kaiwk/slim-modeline
;; Keywords: modeline

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to minimize mode line. Basically it will
;; make mode line look like a straight line, and user can customize its color.

;;; Code:

(defgroup slim-modeline nil
  "Make mode-line slim, and look like disappeared"
  :prefix "smm-"
  :group 'mode-line)

(defcustom smm-slim-modeline-color "darkred"
  "Slim mode line color."
  :type 'string
  :group 'slim-modeline)

(defvar smm-foreground-backup nil
  "Mode line foreground backup.")

(defvar smm-background-backup nil
  "Mode line background backup.")

(defvar smm-inactive-foreground-backup nil
  "Mode line inactive foreground backup.")

(defvar smm-inactive-background-backup nil
  "Mode line inactive background backup.")

(defvar smm-format-backup nil
  "Mode line background.")

(defun smm-save-and-revert-all-file-buffers ()
  "Save and revert all file buffers. This function used to
 update `mode-line-format',  which is a buffer local variable."
  (save-some-buffers t)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when (and filename
                 (file-readable-p filename))
        (with-current-buffer buf
          (revert-buffer :ignore-auto :noconfirm))))))

(define-minor-mode slim-modeline-mode
  "Minimize modeline to a line"
  :global t
  (progn
    (if slim-modeline-mode
        (progn
          (setq-default smm-format-backup  mode-line-format)
          (setq-default mode-line-format "")

          (setq smm-foreground-backup (face-attribute 'mode-line :foreground))
          (setq smm-background-backup (face-attribute 'mode-line :background))
          (setq smm-inactive-foreground-backup (face-attribute 'mode-line-inactive :foreground))
          (setq smm-inactive-background-backup (face-attribute 'mode-line-inactive :background))

          (unless smm-slim-modeline-color
            (setq smm-slim-modeline-color smm-background-backup))

          (set-face-attribute 'mode-line nil :background smm-slim-modeline-color)
          (set-face-attribute 'mode-line nil :foreground smm-slim-modeline-color)
          (set-face-attribute 'mode-line-inactive nil :background smm-slim-modeline-color)
          (set-face-attribute 'mode-line-inactive nil :foreground smm-slim-modeline-color)

          (set-face-attribute 'mode-line nil :height 0.1)
          (set-face-attribute 'mode-line-inactive nil :height 0.1))
      (progn
        (setq-default mode-line-format smm-format-backup)

        (set-face-attribute 'mode-line nil :background smm-background-backup)
        (set-face-attribute 'mode-line nil :foreground smm-foreground-backup)
        (set-face-attribute 'mode-line-inactive nil :background smm-inactive-background-backup)
        (set-face-attribute 'mode-line-inactive nil :foreground smm-inactive-foreground-backup)

        (set-face-attribute 'mode-line nil :height 1)
        (set-face-attribute 'mode-line-inactive nil :height 1)))
    (smm-save-and-revert-all-file-buffers)))

(provide 'slim-modeline)