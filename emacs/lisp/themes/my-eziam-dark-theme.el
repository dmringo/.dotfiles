;;; eziam-dark-theme.el --- Dark version of the Eziam theme

;; Copyright (c) 2016-2017 Thibault Polge <thibault@thb.lt>

;; Modificiations:
;;
;; Mar  1, 2018
;;   Fix shitty rainbow delimiters colors

;; Eziam is based on Tao theme, copyright (C) 2014 Peter <11111000000
;; at email.com> with contributions by Jasonm23 <jasonm23@gmail.com>.
;; Tao also credits: "Original faces taken from Zenburn theme port (c)
;; by Bozhidar Batsov"

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintener: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: faces
;; Homepage: https://github.com/thblt/eziam-theme-emacs
;; Version: 0.4.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a dark version of the Eziam theme for Emacs.

;;; Code:

(require 'eziam-common)

(deftheme my-eziam-dark "The dark Eziam color theme - modified")

(eziam-with-color-variables
  (
   ;; Base palette
   ("color-0"                . "#000000")
   ("color-1"                . "#222222")
   ("color-2"                . "#333333")
   ("color-3"                . "#444444")
   ("color-4"                . "#666666")
   ("color-5"                . "#888888")
   ("color-6"                . "#aaaaaa")
   ("color-7"                . "#dddddd")
   ("color-8"                . "#ffffff")
   ;; Headings
   ("ol1-fg"                 . nil)
   ("ol1-bg"                 . "#000000")
   ("ol2-fg"                 . nil)
   ("ol2-bg"                 . "#063545")
   ("ol3-fg"                 . nil)
   ("ol3-bg"                 . "#186b8c")
   ("ol4-fg"                 . "#222222")
   ("ol4-bg"                 . "#1e85ae")
   ("ol5-fg"                 . "#222222")
   ("ol5-bg"                 . "#96a4ab")
   ("ol6-fg"                 . nil)
   ("ol6-bg"                 . nil)
   ("ol7-fg"                 . nil)
   ("ol7-bg"                 . nil)
   ("ol8-fg"                 . nil)
   ("ol8-bg"                 . nil)
   ;; Misc
   ("transient-highlight"    . "yellow")
   ("transient-highlight-fg" . "#000000")
   ("warning"                . "gold")
   ("error"                  . "#ff0000")
   ("info"                   . "DeepSkyBlue")
   ("ok"                     . "green")


   ("rainbow-1"              . "grey55")
   ("rainbow-2"              . "#93a8c6")
   ("rainbow-3"              . "#b0b1a3")
   ("rainbow-4"              . "#97b098")
   ("rainbow-5"              . "#aebed8")
   ("rainbow-6"              . "#b0b0b3")
   )
  (eziam-apply-custom-theme 'my-eziam-dark))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'my-eziam-dark)

;; Local Variables:
;; mode: emacs-lisp;
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; eziam-dark-theme.el ends here
