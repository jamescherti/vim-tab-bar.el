;;; tab-bar-vim.el --- Make the Emacs tab-bar Look Like Vim’s Tab Bar  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/tab-bar-vim.el
;; Keywords: tab-bar
;; Package-Requires: ((emacs "27.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; tab-bar-vim provides a Vim-like tab bar.

;;; Code:

(require 'tab-bar)

(defgroup tab-bar-vim nil
  "Non-nil if tab-bar-vim mode mode is enabled."
  :group 'tab-bar-vim)

(defcustom tab-bar-vim-show-groups nil
  "Show groups in the tab-bar."
  :type 'boolean
  :group 'tab-bar-vim)

(defvar tab-bar-vim--after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun tab-bar-vim--name-format-function (tab i)
  "Format a TAB name of the tab index I like Vim."
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat " "
             (if tab-bar-tab-hints (format "%d " i) "")
             (alist-get 'name tab)
             (or (and tab-bar-close-button-show
                      (not (eq tab-bar-close-button-show
                               (if current-p 'non-selected 'selected)))
                      tab-bar-close-button)
                 "")
             " ")
     'face (funcall tab-bar-tab-face-function tab))))

(defun tab-bar-vim--group-format-function (tab i &optional current-p)
  "Format a TAB group like Vim.
This function spaces around the group name. Index I is included when
`tab-bar-tab-hints' is enabled and CURRENT-P is nil, indicating the tab is not
the current group."
  (propertize
   (concat " "
           (if (and tab-bar-tab-hints (not current-p)) (format "%d " i) "")
           (funcall tab-bar-tab-group-function tab)
           " ")
   'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

(defun tab-bar-vim--apply ()
  "Apply Vim-like color themes to Emacs tab bars."
  (let* ((color-fallback-light (face-attribute 'default :foreground))
         (fallback-color-dark (face-attribute 'default :background))
         (bg-default (or (face-attribute 'default :background)
                         color-fallback-light))
         (fg-default (or (face-attribute 'default :foreground)
                         fallback-color-dark))
         (bg-modeline-inactive (or (face-attribute 'mode-line-inactive :background)
                                   fallback-color-dark))
         (fg-modeline-inactive (or (face-attribute 'mode-line-inactive :foreground)
                                   color-fallback-light))
         (bg-tab-inactive bg-modeline-inactive)
         (fg-tab-inactive fg-modeline-inactive)
         (fg-tab-active fg-default)
         (bg-tab-active bg-default))
    (setq tab-bar-tab-name-format-function #'tab-bar-vim--name-format-function)
    (setq tab-bar-tab-group-format-function #'tab-bar-vim--group-format-function)
    (if tab-bar-vim-show-groups
        (setq tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))
      (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))
    (with-suppressed-warnings ((obsolete tab-bar-new-button-show))
      (setq tab-bar-new-button-show nil))  ; Obsolete variable as of Emacs 28.1
    (setq tab-bar-separator "\u200B")  ;; Zero width space to fix color bleeding
    (setq tab-bar-tab-hints nil)  ;; Tab numbers of the left of the label
    (setq tab-bar-close-button-show nil)
    (setq tab-bar-auto-width nil)
    (custom-set-faces
     ;; The tab bar's appearance
     `(tab-bar
       ((t (:background ,bg-tab-inactive
                        :foreground ,fg-tab-inactive
                        :box (:line-width 3 :color ,bg-tab-inactive :style nil)))))
     ;; Inactive tabs
     `(tab-bar-tab-inactive
       ((t (:background ,bg-tab-inactive
                        :foreground ,fg-tab-inactive
                        :box (:line-width 3 :color ,bg-tab-inactive :style nil)))))
     ;; Active tab
     `(tab-bar-tab
       ((t (:background ,bg-tab-active :foreground ,fg-tab-active
                        :box (:line-width 3 :color ,bg-tab-active :style nil)))))

     ;; The tab bar's appearance
     `(tab-bar-tab-ungrouped
       ((t (:background ,bg-tab-inactive
                        :foreground ,fg-tab-inactive
                        :box (:line-width 3 :color ,bg-tab-inactive :style nil)))))

     ;; Inactive tabs
     `(tab-bar-tab-group-inactive
       ((t (:background ,bg-tab-inactive
                        :foreground ,fg-tab-inactive
                        :box (:line-width 3 :color ,bg-tab-inactive :style nil)))))

     ;; Active tab
     `(tab-bar-tab-group-current
       ((t (:background ,bg-tab-inactive :foreground ,fg-tab-active
                        :box (:line-width 3 :color ,bg-tab-inactive :style nil)))))))

  (tab-bar-mode 1))

(defun tab-bar-vim--after-load-theme-hook (&rest _args)
  "Run `tab-bar-vim--after-load-theme-hook'."
  (run-hooks 'tab-bar-vim--after-load-theme-hook))

;;;###autoload
(define-minor-mode tab-bar-vim-mode
  "Toggle `tab-bar-vim-mode'."
  :global t
  :lighter " TB-Vim"
  :group 'tab-bar-vim
  (if tab-bar-vim-mode
      (progn
        (advice-add 'load-theme :after #'tab-bar-vim--after-load-theme-hook)
        (tab-bar-vim--apply)
        (add-hook 'tab-bar-vim--after-load-theme-hook 'tab-bar-vim--apply))
    (advice-remove 'load-theme #'tab-bar-vim--after-load-theme-hook)))

(provide 'tab-bar-vim)
;;; tab-bar-vim.el ends here
