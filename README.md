# vim-tab-bar.el
[![MELPA](https://melpa.org/packages/vim-tab-bar-badge.svg)](https://melpa.org/#/vim-tab-bar)
![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/made-for-gnu-emacs.svg)
![License](https://img.shields.io/github/license/jamescherti/vim-tab-bar.el)

The `vim-tab-bar.el` package enhances Emacs' built-in tab-bar, giving it a style similar to Vim's tabbed browsing interface. It also ensures that the tab-bar's appearance aligns with the current theme's overall color scheme, providing a consistent look for your Emacs tab-bar.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Options](#options)
- [Screenshots](#screenshots)
- [License](#license)
- [Links](#links)

## Features

- The `vim-tab-bar.el` package ensures that tabs maintain a uniform appearance regardless of the theme used. This is also beneficial those who use themes that do not configure the tab-bar properly.
- Group Formatting: Optionally display and format tab groups.
- Makes the Emacs `tab-bar` (Emacs version >= 27.1) look in a manner reminiscent of Vim's tabbed browsing interface:
![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim.png)

(The screenshot above shows how the `vim-tab-bar.el` package makes the Emacs `tab-bar` appear)

## Installation

The `vim-tab-bar` package can be installed from MELPA by adding the following to your init.el:
```emacs-lisp
(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :hook
  (after-init . vim-tab-bar-mode))
```

## Options

### Show the tab groups

By default, tab groups are not displayed. You can make `tab-bar-vim` display them by setting the variable:
``` emacs-lisp
(setq vim-tab-bar-show-groups t)
```

###

## Screenshots

The screenshots below shows how the `vim-tab-bar.el` package makes the Emacs `tab-bar` appear:

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

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
