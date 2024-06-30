# vim-tab-bar.el
[![MELPA](https://melpa.org/packages/vim-tab-bar-badge.svg)](https://melpa.org/#/vim-tab-bar)

The `vim-tab-bar.el` package enhances Emacs' built-in tab-bar, giving it a style similar to Vim's tabbed browsing interface. It also ensures that the tab-bar's appearance aligns with the current theme's overall color scheme, providing a consistent look for your Emacs tab-bar.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Options](#options)
- [Screenshots](#screenshots)
- [License](#license)
- [Links](#links)

## Features

- The `vim-tab-bar.el` package ensures that tabs maintain a uniform appearance regardless of the theme used. This is beneficial those who use themes that do not configure the tab-bar properly.
- Group Formatting: Optionally display and format tab groups.
- Makes the Emacs `tab-bar` (Emacs version >= 27.1) look in a manner reminiscent of Vim's tabbed browsing interface:
![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim.png)

(The screenshot above shows how the Emacs `vim-tab-bar.el` package makes the `tab-bar` appear)

## Installation

The `vim-tab-bar` package can be installed from MELPA by adding the following to your init.el:
```emacs-lisp
(use-package vim-tab-bar
  :ensure t
  :config
  (add-hook 'after-init-hook #'vim-tab-bar-mode))
```

## Options

Customize `vim-tab-bar` by setting the following options in your Emacs configuration to suit your preferences:
``` emacs-lisp
;; vim-tab-bar-show-groups: Determines whether tab groups are displayed.
;; Default is nil (groups not displayed).
(setq vim-tab-bar-show-groups nil)
```

## Screenshots

![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim-tomorrow-night-deepblue.png)

![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim-tango-dark.png)

![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim.png)

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [vim-tab-bar.el @GitHub](https://github.com/jamescherti/vim-tab-bar.el)
- [Article: Making the Emacs built-in tab-bar Look Like Vim’s Tab Bar](https://www.jamescherti.com/emacs-tab-bar-vim-style-colors/)
-  [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes.
