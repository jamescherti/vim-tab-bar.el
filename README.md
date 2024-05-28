# vim-tab-bar.el

## Overview

The `vim-tab-bar.el` package enhances Emacs' built-in tab-bar, giving it a style similar to Vim's tabbed browsing interface. It also ensures that the tab-bar's appearance aligns with the current theme's overall color scheme.

This is how the Emacs `vim-tab-bar.el` package makes the `tab-bar` appear:
![](https://raw.githubusercontent.com/jamescherti/vim-tab-bar.el/main/.screenshots/emacs-tab-like-vim.png)

The Emacs built-in `tab-bar` feature, which enables users to manage multiple buffers through a visual interface at the top of the Emacs window, is available in Emacs version 27.1 or higher.

## Features

- Vim-like tab bar: Makes the Emacs `tab-bar` look in a manner reminiscent of Vim's tabbed browsing interface.
- Theme Compatibility: Automatically applies Vim-like color themes to tab bars, responding dynamically to theme changes in Emacs.
- Group Formatting: Optionally display and format tab groups.

## Installation

1. Clone the `vim-tab-bar` repository to your local machine and add it to your `load-path`.
2. Add the following lines to your Emacs configuration:

```emacs-lisp
(require 'vim-tab-bar)
(vim-tab-bar-mode 1)
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

Copyright (c) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Related links

- [vim-tab-bar.el @GitHub](https://github.com/jamescherti/vim-tab-bar.el)
- [Article: Making the Emacs built-in tab-bar Look Like Vim’s Tab Bar](https://www.jamescherti.com/emacs-tab-bar-vim-style-colors/)
