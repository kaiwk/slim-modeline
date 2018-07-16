;;; slim-modeline --- A package provides functionality to minimize mode line.

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
;; make mode line look like a straight line.

;;; Code:

(defgroup slim-modeline nil
  "Make mode-line slim, and look like disappeared"
  :prefix "smm-"
  :group 'mode-line)

(defvar smm--modeline-format-backup nil
  "slim mode line format backup")

(defun smm--slim-modeline-enabled-p ()
  (member 'slim-modeline custom-enabled-themes))

(defun smm--save-and-revert-all-file-buffers ()
  "Save and revert all file buffers. This function used to
 update `mode-line-format',  which is a buffer local variable."
  (save-some-buffers t)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when (and filename
                 (file-readable-p filename))
        (with-current-buffer buf
          (revert-buffer :ignore-auto :noconfirm))))))

;;;###autoload
(defun toggle-slim-modeline ()
  (interactive)
  (if (smm--slim-modeline-enabled-p)
      (progn
        (setq-default mode-line-format smm--modeline-format-backup)
        (disable-theme 'slim-modeline))
    (progn
      (setq smm--modeline-format-backup mode-line-format)
      (setq-default mode-line-format "")
      (load-theme 'slim-modeline t)))
  (smm--save-and-revert-all-file-buffers))

(provide 'slim-modeline)
