# Prettier-RC for Emacs

[![MELPA](http://melpa.org/packages/prettier-rc-badge.svg)](http://melpa.org/#/prettier-rc)

prettier-rc is a function that searches for local rc rules and editorconfig rules per project and use it to format the current buffer using
[prettier](https://github.com/prettier/prettier). The package also exports a minor mode that applies `(prettier-rc)` on save.

## Configuration

### Requirements

By default, it will use the local prettier is installed via `npm`, if it is not
found, it will fallback to the prettier found in the PATH.

Make sure that the prettier program is installed:

```bash
which prettier
```

If prettier is not installed already, you can install prettier using `npm install -g prettier` or via your package manager.

### Basic configuration

First require the package:

```elisp
(require 'prettier-rc)
```

Then you can hook to your favorite javascript/typescript mode:

```elisp
(add-hook 'typescript-mode-hook 'prettier-rc-mode)
(add-hook 'js2-mode-hook 'prettier-rc-mode)
(add-hook 'web-mode-hook 'prettier-rc-mode)
...
```

## Customization

This package is customizable via custom.el:

```
M-x customize-group prettier-js
```

- `prettier-rc-skip-package-json` Do not use the package.json configuration file (t or nil)
- `prettier-rc-skip-editorconfig` Do not use the .editorconfig configuration file (t or nil)
- `prettier-rc-use-local-prettier` Use the node_modules prettier, fallback to PATH if not found (t or nil)
