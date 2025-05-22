;;; vim-tab-bar.el --- Vim-like tab bar -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.7
;; URL: https://github.com/jamescherti/vim-tab-bar.el
;; Keywords: frames
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; The `vim-tab-bar.el` package enhances Emacs' built-in tab-bar, giving it a
;; style similar to Vim's tabbed browsing interface. It also ensures that the
;; tab-bar's appearance aligns with the current theme's overall color scheme.
;;
;; Features:
;; - Vim-like tab bar: Makes the Emacs `tab-bar` look in a manner reminiscent
;;   of Vim's tabbed browsing interface.
;; - Theme Compatibility: Automatically applies Vim-like color themes to tab
;;   bars, responding dynamically to theme changes in Emacs.
;; - Group Formatting: Optionally display and format tab groups.
;;
;; Installation:
;; -------------
;; (use-package vim-tab-bar
;;   :ensure t
;;   :commands vim-tab-bar-mode
;;   :hook
;;   (after-init . vim-tab-bar-mode))
;;
;; Links:
;; ------
;; - vim-tab-bar.el @GitHub (Screenshots, usage, etc.):
;;   https://github.com/jamescherti/vim-tab-bar.el


;;; Code:

(require 'tab-bar)

(defgroup vim-tab-bar nil
  "Non-nil if vim-tab-bar mode mode is enabled."
  :group 'vim-tab-bar
  :prefix "vim-tab-bar-"
  :link '(url-link "https://github.com/jamescherti/vim-tab-bar.el"))

(defcustom vim-tab-bar-show-groups nil
  "Show groups in the tab-bar."
  :type 'boolean
  :group 'vim-tab-bar)

(defun vim-tab-bar--default-format-tab-name (name)
  "Return NAME surrounded by spaces."
  (concat " " name " "))

(defun vim-tab-bar--default-format-group-name (name)
  "Return NAME surrounded by spaces."
  (concat " " name " "))

(defcustom vim-tab-bar-update-tab-name-function
  #'vim-tab-bar--default-format-tab-name
  "Function to format the name of a tab.
The function should take a string as its sole argument and return a string."
  :type 'function
  :group 'vim-tab-bar)

(defcustom vim-tab-bar-update-group-name-function
  #'vim-tab-bar--default-format-group-name
  "Function to format the name of a tab group.
The function should take a string as its sole argument and return a string."
  :type 'function
  :group 'vim-tab-bar)

(defvar vim-tab-bar--after-load-theme-hook nil
  "Hook run after `load-theme' to update the tab-bar faces.")

(defun vim-tab-bar--name-format-function (tab i)
  "Format a TAB name of the tab index I like Vim."
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (funcall vim-tab-bar-update-tab-name-function
              (concat
               (if tab-bar-tab-hints (format "%d " i) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   "")))
     'face (funcall tab-bar-tab-face-function tab))))

(defun vim-tab-bar--group-format-function (tab i &optional current-p)
  "Format a TAB group like Vim.
This function spaces around the group name. Index I is included when
`tab-bar-tab-hints' is enabled and CURRENT-P is nil, indicating the tab is not
the current group."
  (propertize
   (funcall vim-tab-bar-update-group-name-function
            (concat
             (if (and tab-bar-tab-hints (not current-p))
                 (format "%d " i)
               "")
             (funcall tab-bar-tab-group-function tab)))
   'face (if current-p 'tab-bar-tab-group-current
           'tab-bar-tab-group-inactive)))

(defun vim-tab-bar--apply (&optional frame)
  "Apply Vim-like color themes to Emacs tab bars.
FRAME is the frame."
  (unless frame
    (setq frame (selected-frame)))
  (let* ((color-fallback-light (face-attribute 'default :foreground))
         (fallback-color-dark (face-attribute 'default :background))
         (bg-default (or (face-attribute 'default :background)
                         color-fallback-light))
         (fg-default (or (face-attribute 'default :foreground)
                         fallback-color-dark))
         (bg-modeline-inactive (or (face-attribute
                                    'mode-line-inactive :background)
                                   fallback-color-dark))
         (fg-modeline-inactive (or (face-attribute
                                    'mode-line-inactive :foreground)
                                   color-fallback-light))
         (bg-tab-inactive bg-modeline-inactive)
         (fg-tab-inactive fg-modeline-inactive)
         (fg-tab-active fg-default)
         (bg-tab-active bg-default))
    (setq tab-bar-tab-name-format-function
          #'vim-tab-bar--name-format-function)
    (setq tab-bar-tab-group-format-function
          #'vim-tab-bar--group-format-function)
    (if vim-tab-bar-show-groups
        (setq tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))
      (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))
    (with-suppressed-warnings ((obsolete tab-bar-new-button-show))
      (setq tab-bar-new-button-show nil))  ; Obsolete as of Emacs 28.1
    (setq tab-bar-separator "\u200B")  ; Zero width space to fix color bleeding
    (setq tab-bar-tab-hints nil)  ; Tab numbers of the left of the label
    (setq tab-bar-close-button-show nil)
    (setq tab-bar-auto-width nil)

    ;; The tab bar's appearance
    (set-face-attribute 'tab-bar frame
                        :background bg-tab-inactive
                        :foreground fg-tab-inactive
                        :inverse-video 'unspecified
                        :inherit 'unspecified
                        :family 'unspecified
                        :foundry 'unspecified
                        :width 'unspecified
                        :height 'unspecified
                        :weight 'unspecified
                        :slant 'unspecified
                        :underline 'unspecified
                        :overline 'unspecified
                        :extend 'unspecified
                        :strike-through 'unspecified
                        :stipple 'unspecified)

    ;; Inactive tabs
    (set-face-attribute 'tab-bar-tab-inactive frame
                        :background bg-tab-inactive
                        :foreground fg-tab-inactive
                        :inverse-video 'unspecified
                        :inherit 'unspecified
                        :family 'unspecified
                        :foundry 'unspecified
                        :width 'unspecified
                        :height 'unspecified
                        :weight 'unspecified
                        :slant 'unspecified
                        :underline 'unspecified
                        :overline 'unspecified
                        :extend 'unspecified
                        :strike-through 'unspecified
                        :stipple 'unspecified)

    ;; Active tab
    (set-face-attribute 'tab-bar-tab frame
                        :background bg-tab-active
                        :foreground fg-tab-active
                        :inverse-video 'unspecified
                        :inherit 'unspecified
                        :family 'unspecified
                        :foundry 'unspecified
                        :width 'unspecified
                        :height 'unspecified
                        :weight 'unspecified
                        :slant 'unspecified
                        :underline 'unspecified
                        :overline 'unspecified
                        :extend 'unspecified
                        :strike-through 'unspecified
                        :stipple 'unspecified)

    ;; The tab bar's ungrouped appearance
    (set-face-attribute 'tab-bar-tab-ungrouped frame
                        :background bg-tab-inactive
                        :foreground fg-tab-inactive
                        :inverse-video 'unspecified
                        :inherit 'unspecified
                        :family 'unspecified
                        :foundry 'unspecified
                        :width 'unspecified
                        :height 'unspecified
                        :weight 'unspecified
                        :slant 'unspecified
                        :underline 'unspecified
                        :overline 'unspecified
                        :extend 'unspecified
                        :strike-through 'unspecified
                        :stipple 'unspecified)

    ;; Inactive tab groups
    (set-face-attribute 'tab-bar-tab-group-inactive frame
                        :background bg-tab-inactive
                        :foreground fg-tab-inactive
                        :inverse-video 'unspecified
                        :inherit 'unspecified
                        :family 'unspecified
                        :foundry 'unspecified
                        :width 'unspecified
                        :height 'unspecified
                        :weight 'unspecified
                        :slant 'unspecified
                        :underline 'unspecified
                        :overline 'unspecified
                        :extend 'unspecified
                        :strike-through 'unspecified
                        :stipple 'unspecified)

    ;; Current tab group
    (set-face-attribute 'tab-bar-tab-group-current frame
                        :background bg-tab-inactive
                        :foreground fg-tab-active
                        :inverse-video 'unspecified
                        :inherit 'unspecified
                        :family 'unspecified
                        :foundry 'unspecified
                        :width 'unspecified
                        :height 'unspecified
                        :weight 'unspecified
                        :slant 'unspecified
                        :underline 'unspecified
                        :overline 'unspecified
                        :extend 'unspecified
                        :strike-through 'unspecified
                        :stipple 'unspecified)

    (when (and (display-graphic-p)
               (not (daemonp)))
      (set-face-attribute
       'tab-bar frame
       :box `(:line-width 3 :color ,bg-tab-inactive :style nil))
      (set-face-attribute
       'tab-bar-tab-inactive frame
       :box `(:line-width 3 :color ,bg-tab-inactive :style nil))
      (set-face-attribute
       'tab-bar-tab frame
       :box `(:line-width 3 :color ,bg-tab-active :style nil))
      (set-face-attribute
       'tab-bar-tab-ungrouped frame
       :box `(:line-width 3 :color ,bg-tab-inactive :style nil))
      (set-face-attribute
       'tab-bar-tab-group-inactive frame
       :box `(:line-width 3 :color ,bg-tab-inactive :style nil))
      (set-face-attribute
       'tab-bar-tab-group-current frame
       :box `(:line-width 3 :color ,bg-tab-inactive :style nil)))))

(defun vim-tab-bar--run-after-load-theme-hook (&rest _args)
  "Run the hooks that are in `vim-tab-bar--after-load-theme-hook'."
  (run-hooks 'vim-tab-bar--after-load-theme-hook))

;;;###autoload
(define-minor-mode vim-tab-bar-mode
  "Toggle `vim-tab-bar-mode'."
  :global t
  :lighter " VimTB"
  :group 'vim-tab-bar
  (if vim-tab-bar-mode
      (progn
        (vim-tab-bar--apply)
        (advice-add 'load-theme :after #'vim-tab-bar--run-after-load-theme-hook)
        ;; Hooks
        (add-hook 'after-make-frame-functions #'vim-tab-bar--apply)
        (add-hook 'vim-tab-bar--after-load-theme-hook #'vim-tab-bar--apply)
        (tab-bar-mode 1))
    (advice-remove 'load-theme #'vim-tab-bar--run-after-load-theme-hook)
    (remove-hook 'after-make-frame-functions #'vim-tab-bar--apply)
    (remove-hook 'vim-tab-bar--after-load-theme-hook #'vim-tab-bar--apply)
    (tab-bar-mode -1)))

(provide 'vim-tab-bar)
;;; vim-tab-bar.el ends here
