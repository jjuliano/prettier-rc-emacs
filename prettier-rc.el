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
;;     defined `.prettierrc.*`, `prettier.config.*`, `.prettierignore` and
;;     `.editorconfig` rules and automatically pass them to Prettier on the
;;     current buffer.
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

(defgroup prettier-rc nil
  "Minor mode to format JS code on file save using local rc rules"
  :group 'languages
  :prefix 'prettier-rc
  :link '(url-link :tag "Repository"
                   "https://github.com/jjuliano/prettier-rc-emacs"))

(defcustom prettier-rc-use-package-json t
  "If non-nil, `prettier-rc' will use `package.json' file."
  :type 'boolean
  :group 'prettier-rc)

(defcustom prettier-rc-use-editorconfig t
  "If non-nil, `prettier-rc' will use `.editorconfig' file."
  :type 'boolean
  :group 'prettier-rc)

(defcustom prettier-rc-use-prettierignore t
  "If non-nil, `prettier-rc' will use `.prettierignore' file."
  :type 'boolean
  :group 'prettier-rc)

(defcustom prettier-rc-use-node-modules-bin t
  "If non-nil, `prettier-rc' will search `node_modules' for `prettier' bin."
  :type 'boolean
  :group 'prettier-rc)

(defun prettier-rc ()
  "Format the current buffer using `prettier-rc' using the defined rc rules."
  (interactive)

  (let (args)
    (flet ((prettier-rc--add-file (file) ;; Builds and store the local rc FILE list if found.
                                  (list :file (concat (locate-dominating-file default-directory file)
                                                      file)))
           (prettier-rc--find-file (file) ;; Search the local base directory for local FILE and store to list.
                                   (if (bound-and-true-p file)
                                       (if (locate-dominating-file default-directory file)
                                           (append (prettier-rc--add-file file)))))
           (prettier-rc--build-config (file) ;; Build the config arguments
                                      (if (prettier-rc--find-file file)
                                          (cond ((string= file ".editorconfig") ;; check if `.editorconfig' will be skipped
                                                 (if (bound-and-true-p prettier-rc-use-editorconfig)
                                                     (push (concat "--config " (concat (locate-dominating-file
                                                                                        default-directory file) file))
                                                           args)))
                                                ;; check if `package.json' will be skipped
                                                ((string= file "package.json")
                                                 (if (bound-and-true-p prettier-rc-use-package-json)
                                                     (push (concat "--config " (concat (locate-dominating-file
                                                                                        default-directory file) file))
                                                           args)))
                                                ;; check if `.prettierignore' will be skipped
                                                ((string= file ".prettierignore")
                                                 (if (bound-and-true-p prettier-rc-use-prettierignore)
                                                     (push (concat "--ignore-path " (concat (locate-dominating-file
                                                                                        default-directory file) file))
                                                           args)))
                                                ;; append the rc file to the list when found
                                                (t (push (concat "--config " (concat (locate-dominating-file
                                                                                      default-directory file) file))
                                                         args))))))

      (mapcar #'(lambda (rc)
                  (prettier-rc--build-config rc))
              (list ".prettierrc"
                    ".prettierrc.json"
                    ".prettierrc.yaml"
                    ".prettierrc.yml"
                    ".prettierrc.json5"
                    ".prettierrc.js"
                    ".prettierrc.cjs"
                    "prettier.config.js"
                    "prettier.config.cjs"
                    ".prettierrc.toml"
                    "package.json"
                    ".prettierignore"
                    ".editorconfig")))

    ;; only specify prettier-js-args-options if files are found
    (if (bound-and-true-p args)
        (setq prettier-js-args (remove nil
                                       `(,(unless (bound-and-true-p prettier-rc-use-editorconfig)
                                            "--no-editorconfig")
                                         ,(mapconcat #'identity args " ")
                                         "--write")))
      ;; cleanup args
      (setq prettier-js-args '())))

  ;; check if prefer to use local prettier via `npm'
  (progn
    (if prettier-rc-use-node-modules-bin
        (let* ((file-name (or (buffer-file-name) default-directory))
               (root (locate-dominating-file file-name "node_modules"))
               (prettier (and root
                              (expand-file-name "node_modules/.bin/prettier" root))))
          (if (and prettier (file-executable-p prettier))
              (setq prettier-js-command prettier)))))

  ;; finally call prettier-js
  (unless (string= prettier-js-command "prettier")
    (message "Found prettier binary in `%s'." prettier-js-command))
  (prettier-js))

;;;###autoload
(define-minor-mode prettier-rc-mode
  "Runs prettier on file save using local rc rules when this mode is turned on"
  :lighter " Prettier-RC"
  :global nil
  ;; Toggle prettier-rc-mode
  (if prettier-rc-mode
      (add-hook 'before-save-hook #'prettier-rc nil t)
    (remove-hook 'before-save-hook #'prettier-rc t)))

(provide 'prettier-rc)
;;; prettier-rc.el ends here
