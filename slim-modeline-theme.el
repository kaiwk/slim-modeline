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

(deftheme slim-modeline "The Monokai colour theme")

(custom-theme-set-faces
 'slim-modeline
 `(mode-line ((t (:height 0.1))))
 `(mode-line-inactive ((t (:foreground ,(face-attribute 'mode-line :background) :height 0.1)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'slim-modeline)
