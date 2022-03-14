;;; prettier-rc.el --- Minor mode for prettier to use local rc rules

;; Copyright (C) 2022-2023  Joel Bryan Juliano

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Author: Joel Bryan Juliano <joelbryan dot juliano at gmail dot com>
;; Created: 08 March 2022
;; URL: https://github.com/jjuliano/prettier-rc-emacs
;; Package-Requires: ((emacs "24") (prettier-js "0.1.0"))
;; Version: 0.1.0
;; Keywords: convenience edit js ts rc prettierrc prettier-rc prettier prettier-js

;;; Commentary:

;; Formats your JavaScript & Typescript code using Prettier and defined rc rules.

;; Usage
;; -----
;;
;;     Running `prettier-rc` will look on the current project's folder for any
;;     defined `.prettierrc.*`, `prettier.config.*` and `.editorconfig` rules
;;     and automatically pass them to Prettier on the current buffer.
;;
;;       M-x prettier-rc
;;
;;     To automatically format after saving:
;;
;;       (add-hook 'typescript-mode-hook 'prettier-rc-mode)
;;       (add-hook 'js2-mode-hook 'prettier-rc-mode)
;;       (add-hook 'web-mode-hook 'prettier-rc-mode)

;;; Code:

(require 'prettier-js)

;; make free variable as buffer local
(make-variable-buffer-local 'prettier-js-args)
(make-variable-buffer-local 'prettier-js-command)
(make-variable-buffer-local 'prettier-rc-skip-editorconfig)
(make-variable-buffer-local 'prettier-rc-use-node-modules-bin)
(make-variable-buffer-local 'prettier-rc-skip-package-json)

(defgroup prettier-rc nil
  "Minor mode to format JS code on file save using local rc rules"
  :group 'languages
  :prefix "prettier-rc"
  :link '(url-link :tag "Repository"
                   "https://github.com/jjuliano/prettier-rc-emacs"))

(defcustom prettier-rc-skip-package-json nil
  "Do not use the `package.json' configuration file."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'prettier-rc)

(defcustom prettier-rc-skip-editorconfig nil
  "Do not use the `.editorconfig' configuration file."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'prettier-rc)

(defcustom prettier-rc-use-node-modules-bin t
  "Use the `node_modules' prettier, fallback to 'PATH' if not found."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'prettier-rc)

(defun prettier-rc ()
  "Format the current buffer using `prettier-rc' using the defined rc rules."
  (interactive)

  (defun prettier-rc--add-file (file)
    "Builds and store the local rc FILE list if found."
    (list :file (concat (locate-dominating-file default-directory file)
                        file)))

  (defun prettier-rc--find-file (file)
    "Search the local base directory for local FILE and store to list."
    (if (locate-dominating-file default-directory file)
        (append (prettier-rc--add-file file))))

  (setq prettier-rc--config-files '(".prettierrc"
                                    ".prettierrc.json"
                                    ".prettierrc.yaml"
                                    ".prettierrc.yml"
                                    ".prettierrc.json5"
                                    ".prettierrc.js"
                                    ".prettierrc.cjs"
                                    "prettier.config.js"
                                    "prettier.config.cjs"
                                    ".prettierrc.toml"))

  ;; check if `package.json' will be skipped
  (unless (bound-and-true-p prettier-rc-skip-package-json)
    (add-to-list 'prettier-rc--config-files "package.json"))

  ;; check if `.editorconfig' will be skipped
  (unless (bound-and-true-p prettier-rc-skip-editorconfig)
    (add-to-list 'prettier-rc--config-files ".editorconfig"))

  ;; check if prefer to use local prettier via `npm'
  (if (bound-and-true-p prettier-rc-use-node-modules-bin)
      (progn
        (let* ((file-name (or (buffer-file-name) default-directory))
               (root (locate-dominating-file file-name "node_modules"))
               (prettier (and root
                              (expand-file-name "node_modules/.bin/prettier" root))))

          (if (and prettier (file-executable-p prettier))
              (progn
                (setq prettier-js-command prettier))
            (progn
              (setq prettier-js-command "prettier"))))))

  (let (args)
    ;; iterate over the local rc files
    (dolist (rc prettier-rc--config-files)
      (if (prettier-rc--find-file rc)
          ;; append the rc file to the list when found
          (push (concat "--config " (concat (locate-dominating-file
                                             default-directory rc) rc))
                args)))

    ;; only specify prettier-js-args if files are found
    (if (bound-and-true-p args)
        (progn
          (setq prettier-js-args (remove nil
                                         `(,(if (bound-and-true-p prettier-rc-skip-editorconfig)
                                                "--no-editorconfig")
                                           ,(mapconcat #'identity args " ")
                                           "--write"))))
      (progn
        ;; cleanup args
        (setq prettier-js-args '()))))

  ;; finally call prettier-js
  (prettier-js))

;;;###autoload
(define-minor-mode prettier-rc-mode
  "Runs prettier on file save using local rc rules when this mode is turned on"
  :lighter " Prettier-RC"
  :global nil
  ;; Toggle prettier-rc-mode
  (if prettier-rc-mode
      (add-hook 'before-save-hook #'prettier-rc nil 'local)
    (remove-hook 'before-save-hook #'prettier-rc 'local)))

(provide 'prettier-rc)
;;; prettier-rc.el ends here
