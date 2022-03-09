;;; prettier-rc.el --- Minor mode for prettier-js to use local rc rules

;; Version: 0.1.0

;; Copyright (C) 2022-2023  Joel Bryan Juliano

;; This file is not part of GNU Emacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;; * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)

;; Author: Joel Bryan Juliano <joelbryan dot juliano at gmail dot com>
;; Created: 08 March 2022
;; URL: https://github.com/jjuliano/prettier-rc-emacs
;; Package-Requires: ((emacs "26") (prettier-js "0.1.0"))
;; Version: 1.0.0
;; Keywords: convenience edit js rc prettierrc prettier-rc prettier prettier-js

;;; Commentary:

;; Formats your JavaScript code using 'prettier' on file save using local rc
;; rules.

;;; Code:

(require 'prettier-js)

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

(defcustom prettier-rc-use-local-prettier t
  "Use the `node_modules' prettier, fallback to 'PATH' if not found."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'prettier-rc)

(defun prettier-rc--add-file (file)
  "Builds and store the local rc FILE list if found."
  (list :file (concat (locate-dominating-file default-directory file)
                      file)))

(defun prettier-rc--find-file (file)
  "Search the local base directory for local FILE and store to list."
  (if (locate-dominating-file default-directory file)
      (append (prettier-rc--add-file file))))

(defun prettier-rc ()
  "Format the current buffer using `prettier-rc' using the defined rc rules."
  (interactive)

  (setq prettier-rc--config-files '("package.json"
                                    ".prettierrc"
                                    ".prettierrc.json"
                                    ".prettierrc.yaml"
                                    ".prettierrc.yml"
                                    ".prettierrc.json5"
                                    ".prettierrc.js"
                                    ".prettierrc.cjs"
                                    "prettier.config.js"
                                    "prettier.config.cjs"
                                    ".prettierrc.toml"
                                    ".editorconfig"))

  (if (bound-and-true-p prettier-rc-skip-package-json)
      (delete "package.json" prettier-rc--config-files))

  (if (bound-and-true-p prettier-rc-skip-editorconfig)
      (delete ".editorconfig" prettier-rc--config-files))

  (if (bound-and-true-p prettier-rc-use-local-prettier)
      (progn
        (let* ((file-name (or (buffer-file-name) default-directory))
               (root (locate-dominating-file file-name "node_modules"))
               (prettier (and root
                              (expand-file-name "node_modules/.bin/prettier" root))))
          (if (and prettier (file-executable-p prettier))
              (progn
                (setq prettier-js-command prettier)
                (make-variable-buffer-local 'prettier-js-command))
            (progn
              (setq prettier-js-command "prettier")
              (make-variable-buffer-local 'prettier-js-command))))))

  (let (args)
    ;; iterate over the local rc files
    (dolist (rc prettier-rc--config-files)
      (if (prettier-rc--find-file rc)
          ;; append the rc file to the list when found
          (push (concat "--config " (concat (locate-dominating-file
                                             default-directory rc) rc)) args)))
    ;; only specify prettier-js-args if files are found
    (if (bound-and-true-p args)
        (progn
          (setq prettier-js-args (remove nil
                                         `(,(if (bound-and-true-p prettier-rc-skip-editorconfig)
                                                "--no-editorconfig")
                                           ,(mapconcat 'identity args " ")
                                           "--write"))))
      (progn
        ;; cleanup args
        (setq prettier-js-args '())
        (make-variable-buffer-local 'prettier-js-args))))

  ;; finally call prettier-js
  (prettier-js))

;;;###autoload
(define-minor-mode prettier-rc-mode
  "Runs prettier on file save using local rc rules when this mode is turned on"
  :lighter " Prettier-RC"
  :global nil
  ;; Toggle prettier-rc-mode
  (if prettier-rc-mode
      (add-hook 'before-save-hook 'prettier-rc nil 'local)
    (remove-hook 'before-save-hook 'prettier-rc 'local)))

(provide 'prettier-rc)
;;; prettier-rc.el ends here
