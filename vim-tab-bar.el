;;; vim-tab-bar.el --- Make the Emacs tab-bar Look Like Vim’s Tab Bar  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/vim-tab-bar.el
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
;; vim-tab-bar provides a Vim-like tab bar.

;;; Code:

(require 'tab-bar)

(defgroup vim-tab-bar nil
  "Non-nil if vim-tab-bar mode mode is enabled."
  :group 'vim-tab-bar)

(defcustom vim-tab-bar-show-groups nil
  "Show groups in the tab-bar."
  :type 'boolean
  :group 'vim-tab-bar)

(defvar vim-tab-bar--after-load-theme-hook nil
  "Hook run after a theme is loaded using `load-theme' to update the tab-bar faces.")

(defun vim-tab-bar--name-format-function (tab i)
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

(defun vim-tab-bar--group-format-function (tab i &optional current-p)
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

(defun vim-tab-bar--apply ()
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
    (setq tab-bar-tab-name-format-function #'vim-tab-bar--name-format-function)
    (setq tab-bar-tab-group-format-function #'vim-tab-bar--group-format-function)
    (if vim-tab-bar-show-groups
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

(defun vim-tab-bar--run-after-load-theme-hook (&rest _args)
  "Run the hooks that are in the variable `vim-tab-bar--after-load-theme-hook'."
  (run-hooks 'vim-tab-bar--after-load-theme-hook))

;;;###autoload
(define-minor-mode vim-tab-bar-mode
  "Toggle `vim-tab-bar-mode'."
  :global t
  :lighter " TB-Vim"
  :group 'vim-tab-bar
  (if vim-tab-bar-mode
      (progn
        (advice-add 'load-theme :after #'vim-tab-bar--run-after-load-theme-hook)
        (vim-tab-bar--apply)
        (add-hook 'vim-tab-bar--after-load-theme-hook 'vim-tab-bar--apply))
    (advice-remove 'load-theme #'vim-tab-bar--run-after-load-theme-hook)))

(provide 'vim-tab-bar)
;;; vim-tab-bar.el ends here
