# vim-tab-bar.el - A Vim-Inspired Emacs Tab-Bar That Automatically Adapts to Any Theme
![Build Status](https://github.com/jamescherti/vim-tab-bar.el/actions/workflows/melpazoid.yml/badge.svg)
[![MELPA](https://melpa.org/packages/vim-tab-bar-badge.svg)](https://melpa.org/#/vim-tab-bar)
[![MELPA Stable](https://stable.melpa.org/packages/vim-tab-bar-badge.svg)](https://stable.melpa.org/#/vim-tab-bar)
![License](https://img.shields.io/github/license/jamescherti/vim-tab-bar.el)
![](https://jamescherti.com/misc/made-for-gnu-emacs.svg)

The *vim-tab-bar* Emacs package enhances the built-in tab-bar with **a minimalist, Vim-inspired design that automatically adapts to the current Emacs theme** as well as any themes loaded subsequently. This ensures the *tab-bar* integrates with the rest of the Emacs interface, eliminating visual conflicts and making it feel like a natural extension of your Emacs environment.

Beyond its Vim-inspired design, the **vim-tab-bar package is valued by users who prioritize theme consistency**, as it integrates the Emacs tab-bar with any Emacs theme, producing a visually coherent and polished interface.

If *vim-tab-bar* enhances your workflow, please show your support by **⭐ starring vim-tab-bar on GitHub** to help more Emacs users discover its benefits.

![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim.png)

*For users unfamiliar with [Emacs’ built-in tab-bar](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html):* Emacs' *tab-bar* allows users to manage multiple independent workspaces, each comprising its own set of window layouts. This goes beyond simply organizing tabs for different tasks or projects; it enables users to switch between distinct contexts, such as coding, reading documentation, or managing emails, without losing track of their previous work. *It is similar to how web browsers or other editors use tabs, but offers far greater control over workspace organization and layout.*

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Options](#options)
- [Screenshots](#screenshots)
- [License](#license)
- [Links](#links)

## Features

- The *vim-tab-bar* package ensures that tabs maintain a uniform appearance regardless of the theme used. This is also beneficial those who use themes that do not configure the *tab-bar* properly.
- Group Formatting: Optionally display and format tab groups.
- Makes the Emacs *tab-bar* (Emacs version >= 27.1) look in a manner reminiscent of Vim's tabbed browsing interface:
![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim.png)

(The screenshot above shows how the *vim-tab-bar* package makes the Emacs *tab-bar* appear)

## Installation

To install *vim-tab-bar* from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install *vim-tab-bar* from MELPA:
```emacs-lisp
(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :init
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'vim-tab-bar-mode)
    (add-hook 'emacs-startup-hook #'vim-tab-bar-mode)))
```

## Usage

The default keybindings for Emacs’s built-in tab-bar are as follows:

* `C-x t 2`: Create a new tab (`tab-bar-new-tab` / `tab-new`)
* `C-x t b RET`: Switch to a buffer in a new tab (`switch-to-buffer-other-tab`)
* `C-x t f RET`: Open a file in a new tab (`find-file-other-tab`)
* `C-x t d RET`: Open Dired in a new tab (`dired-other-tab`)
* `C-x t t C-x b RET`: Use the tab-bar command prefix, then run any buffer-related command (e.g., `other-tab-prefix`, followed by `switch-to-buffer`)
* `C-x t o` or `C-TAB`: Switch to the next tab (`tab-bar-switch-to-next-tab`)
* `S-C-TAB`: Switch to the previous tab (`tab-bar-switch-to-prev-tab`)
* `C-x t RET`: Switch to a named tab with completion (`tab-switch`)

## Frequently asked questions

### How to Show the tab groups?

By default, tab groups are not displayed. You can make `tab-bar-vim` display them by setting the variable:
``` emacs-lisp
(setq vim-tab-bar-show-groups t)
```

### How to Automatic hide the Vim Tab Bar?

By default, *vim-tab-bar* displays the Vim tab bar whenever it is enabled. To hide the tab bar when only a single tab exists, enable the following option:

```elisp
;; Hide the Vim Tab Bar when only a single tab exists
(customize-set-variable 'tab-bar-show 1)
```

Explanation:
- If `tab-bar-show` is set to t (default), which is the default, automatically enable Vim tab bar when commands that create new window configurations are used, such as `tab-new'.

- If `tab-bar-show` is set to a non-negative integer, display the tab bar only when the number of tabs exceeds that value. In particular, a value of 1 hides the tab bar when only a single tab exists and shows it again when additional tabs are created. When a non-negative integer is used, tab bar visibility is determined independently for each frame, depending on the number of tabs present in that frame and whether it exceeds the specified value.

- If set to nil, keep the tab bar permanently hidden. In this case, persistent named window configurations remain available through keyboard commands such as `tab-new`, `tab-close`, `tab-next`, and `tab-switcher`.

## Screenshots

The screenshots below shows how the *vim-tab-bar* package makes the Emacs *tab-bar* appear:

![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim-tomorrow-night-deepblue.png)

![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim-tango-dark.png)

![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim.png)

## License

Copyright (C) 2024-2026 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [vim-tab-bar.el @GitHub](https://github.com/jamescherti/vim-tab-bar.el)
- [Article: Making the Emacs built-in tab-bar Look Like Vim’s Tab Bar](https://www.jamescherti.com/emacs-tab-bar-vim-style-colors/)
-  [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes.

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
- [kirigami.el](https://github.com/jamescherti/kirigami.el): The *kirigami* Emacs package offers a unified interface for opening and closing folds across a diverse set of major and minor modes in Emacs, including `outline-mode`, `outline-minor-mode`, `outline-indent-mode`, `org-mode`, `markdown-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hs-minor-mode`, `hide-ifdef-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, and `treesit-fold-mode`. With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable folding experience.
