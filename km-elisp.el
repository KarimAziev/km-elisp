;;; km-elisp.el --- Miscellaneous Elisp utils -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-elisp
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Miscellaneous Elisp utils

;;; Code:

(declare-function xref-item-summary "xref")
(declare-function find-library-name "find-fun")
(declare-function feature-file "loadhist")
(declare-function file-dependents "loadhist")
(declare-function file-requires "loadhist")

(defcustom km-elisp-right-partial-functions '(fp-rpartial
                                              igist-rpartial)
  "Symbols representing functions that partially apply arguments from the right.

These functions take a function and some arguments, and return a new function
that applies the original function with the given arguments on the right-hand
side.

For example, `fp-rpartial` and `igist-rpartial` are such functions."
  :group 'km-elisp
  :type '(repeat symbol))

(defcustom km-elisp-left-partial-functions '(apply-partially apply funcall
                                             funcall-interactively fp-partial
                                             fp-compose fp-converge fp-use-with
                                             fp-pipe fp-ignore-args igist-compose)
  "Symbols representing functions that take another function as the first argument.

These functions typically call the given function with the remaining arguments.
For example, `apply-partially', `apply', and `funcall' are such functions."
  :group 'km-elisp
  :type '(repeat symbol))

(defun km-elisp-extend-fnsym-in-current-sexp (result)
  "Extend RESULT to highlight arguments in quoted function calls.

This function modifies the RESULT of `elisp--fnsym-in-current-sexp' to handle
cases where the function being called is partially applied or wrapped in another
function call.

It ensures that the correct function signature is displayed in Eldoc.

This function should be used as a `:filter-return' advice for
`elisp--fnsym-in-current-sexp'.

\\=(advice-add \\='elisp--fnsym-in-current-sexp :filter-return
              \\='km-elisp-extend-fnsym-in-current-sexp)"
  (when-let ((argument (car (last result))))
    (if (<= argument 1)
        result
      (let ((fn-sym (car result)))
        (cond ((memq fn-sym km-elisp-right-partial-functions)
               (let ((sym (save-excursion
                            (let ((parse-sexp-ignore-comments t))
                              (ignore-errors (backward-sexp (1- argument))))
                            (when (looking-at "#")
                              (forward-char 1))
                            (when (looking-at "'")
                              (forward-char 1))
                            (elisp--current-symbol))))
                 (list sym argument)))
              ((memq fn-sym km-elisp-left-partial-functions)
               (save-excursion
                 (let ((parse-sexp-ignore-comments t))
                   (ignore-errors (backward-sexp (1- argument))))
                 (when (looking-at "#")
                   (forward-char 1))
                 (when (looking-at "'")
                   (forward-char 1))
                 (let ((res (list (elisp--current-symbol)
                                  (1- argument))))
                   res)))
              (t result))))))

(defcustom km-elisp-expect-functions-ranges '((igist-compose 1 t)
                                              (fp-compose 1 t)
                                              (fp-partial 1 1)
                                              (fp-rpartial 1 1)
                                              (fp-converge 1 t)
                                              (fp-use-with 1 t)
                                              (fp-pipe 1 t)
                                              (fp-ignore-args 1 1))
  "Alist of macro symbols with ranges indicating expected function positions.

Each entry is a list where the first element is a macro symbol, the second
element is the starting index, and the third element is the ending index or
\\='t\\='for all remaining positions.

This is used to extend elisp completions by advising `elisp--expect-function-p'."
  :group 'km-elisp
  :type '(repeat (list
                  symbol
                  (natnum :tag "Start index")
                  (radio :tag "End index"
                   (natnum :value 1)
                   (const :tag "All" t)))))

(defcustom km-elisp-sexp-to-string-fn #'pp-to-string
  "Function to convert an Emacs Lisp s-expression to a string.

Function to convert an Emacs Lisp s-expression to a string.

The default function is `pp-to-string', which pretty-prints the
s-expression. This function is used in contexts where an s-expression
needs to be displayed as a string, such as in macro expansion results.

The function should accept a single argument, the s-expression, and
return a string representation of it."
  :group 'km-elisp
  :type 'function)


;;;###autoload
(defun km-elisp-macroexpand-sexp-at-point (sexp)
  "Expand SEXP at point."
  (interactive (list (sexp-at-point)))
  (let ((map)
        (str-sexp
         (let ((print-level nil))
           (funcall km-elisp-sexp-to-string-fn
                    (macroexpand-all sexp)))))
    (when-let ((key (this-command-keys-vector)))
      (setq map (make-sparse-keymap map))
      (define-key map key #'km-elisp-macroexpand-sexp-at-point))
    (let ((buffer (get-buffer-create "*km-elisp-macroexpand*")))
      (with-current-buffer buffer
        (with-current-buffer-window
            buffer
            (cons 'display-buffer-in-direction
                  '((window-height . fit-window-to-buffer)
                    (preserve-size . window-preserve-size)))
            (lambda (window _value)
              (with-selected-window window
                (setq buffer-read-only t)
                (let ((inhibit-read-only t)
                      (emacs-lisp-mode-hook nil))
                  (erase-buffer)
                  (emacs-lisp-mode)
                  (when-let* ((quit-key (where-is-internal
                                         'quit-window
                                         special-mode-map
                                         t t t))
                              (map (make-sparse-keymap)))
                    (define-key map quit-key #'quit-window)
                    (use-local-map (make-composed-keymap
                                    map
                                    (current-local-map))))
                  (save-excursion
                    (insert str-sexp))))
              (select-window window)))))))


(defun km-elisp-xref-find-lib (fn symbol)
  "Advice FN to find SYMBOL, or try to find symbol as library.

Usage:

\\=(advice-add \\='elisp--xref-find-definitions :around \\='km-elisp-xref-find-lib)"
  (let ((result (funcall fn symbol)))
    (if (and (car-safe result)
             (> (length result) 1)
             (progn
               (require 'xref)
               (xref-item-summary (car result)))
             (string-match-p "feature"
                             (xref-item-summary (car result))))
        (seq-drop result 1)
      (or result
          (let ((file (ignore-errors
                        (require 'find-func)
                        (find-library-name (symbol-name symbol)))))
            (when file
              (list (elisp--xref-make-xref 'feature symbol file))))))))


(defun km-elisp-extend-expect-function-p (pos)
  "Return non-nil if the symbol at position POS is expected to be a function.

POS is the position in the buffer to check.

This function extends the behavior of `elisp--expect-function-p' to include
additional macros specified in `km-elisp-expect-functions-ranges'.

\\=(advice-add \\='elisp--expect-function-p :after-until
              #\\='km-elisp-extend-expect-function-p)"
  (when-let* ((parent-sexp (save-excursion
                             (let ((parent (nth 1 (syntax-ppss pos))))
                               (when parent
                                 (goto-char parent)
                                 (sexp-at-point)))))
              (parent-cell (assq (car-safe parent-sexp)
                                 km-elisp-expect-functions-ranges)))
    (pcase-let ((`(,_sym ,min-idx ,max-idx) parent-cell))
      (when (eq max-idx t)
        (setq max-idx most-positive-fixnum))
      (save-excursion
        (goto-char pos)
        (let ((count 0)
              (parse-sexp-ignore-comments t))
          (when (sexp-at-point)
            (backward-sexp))
          (while (condition-case nil
                     (progn (backward-sexp 1)
                            (setq count (1+ count)))
                   (error nil)))
          (<= min-idx count max-idx))))))

(defun km-elisp-apply-while-no-input (fn &rest args)
  "Apply function FN with ARGS while no input is pending.

Argument FN is the function to be applied.

This can be useful to prevent performance issues in certain contexts, such as
inside the `pcase' macro, where `elisp-eldoc-funcall' can be extremely slow,
making it difficult to edit.

Remaining arguments ARGS are the arguments to be passed to FN."
  (while-no-input (apply fn args)))

;;;###autoload
(defun km-elisp-advice-eldoc-fns-enable ()
  "Enable advice to extend Eldoc functions for better documentation.

This function adds `km-elisp-extend-fnsym-in-current-sexp' as a :filter-return
advice to `elisp--fnsym-in-current-sexp', enhancing its behavior to handle
additional cases for Eldoc documentation.

It also adds `km-elisp-apply-while-no-input'as an :around advice to
`elisp-eldoc-funcall' to prevent performance issues in certain contexts, such as
inside the `pcase' macro.

The `elisp--fnsym-in-current-sexp' function is used in built-in Emacs Lisp Eldoc
functions to determine the function symbol and argument index at point."
  (interactive)
  (advice-add 'elisp--fnsym-in-current-sexp :filter-return
              #'km-elisp-extend-fnsym-in-current-sexp)
  (when (fboundp 'elisp-eldoc-funcall)
    (advice-add 'elisp-eldoc-funcall :around #'km-elisp-apply-while-no-input)))

;;;###autoload
(defun km-elisp-advice-eldoc-fns-disable ()
  "Disable advice that extends `elisp--fnsym-in-current-sexp'.

This function removes the advice added by `km-elisp-advice-eldoc-fns-enable',
restoring the original behavior of `elisp--fnsym-in-current-sexp' and
`elisp-eldoc-funcall'."
  (interactive)
  (advice-remove 'elisp--fnsym-in-current-sexp
                 #'km-elisp-extend-fnsym-in-current-sexp)
  (when (fboundp 'elisp-eldoc-funcall)
    (advice-remove 'elisp-eldoc-funcall #'km-elisp-apply-while-no-input)))

;;;###autoload
(defun km-elisp-advice-extend-expect-function-enable ()
  "Enable advice to extend `elisp--expect-function-p'.

This advice extends the behavior of `elisp--expect-function-p' to include
additional macros specified in `km-elisp-expect-functions-ranges'. The
`elisp--expect-function-p' function is used in built-in Emacs Lisp completions
to determine if a symbol at a given position is expected to be a function."
  (interactive)
  (advice-add 'elisp--expect-function-p :after-until
              #'km-elisp-extend-expect-function-p))

;;;###autoload
(defun km-elisp-advice-extend-expect-function-disable ()
  "Disable advice that extends `elisp--expect-function-p'.

This removes the advice added by
`km-elisp-advice-extend-expect-function-enable', restoring the original behavior
of `elisp--expect-function-p'."
  (interactive)
  (advice-remove 'elisp--expect-function-p #'km-elisp-extend-expect-function-p))

(defun km-elisp--popup (buff content &optional mode)
  "Display a buffer with CONTENT, optionally using MODE, in a popup window.

Argument BUFF is the name of the buffer to create or use.

Argument CONTENT is the content to display in the buffer.

Optional argument MODE is the major mode to set for the buffer, defaulting to
`emacs-lisp-mode'."
  (let ((buffer (get-buffer-create buff)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-in-direction
                '((window-height . fit-window-to-buffer)
                  (preserve-size . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t)
                    (emacs-lisp-mode-hook nil))
                (erase-buffer)
                (funcall (or mode #'emacs-lisp-mode))
                (when-let* ((quit-key (where-is-internal
                                       'quit-window
                                       special-mode-map
                                       t t t))
                            (map (make-sparse-keymap)))
                  (define-key map quit-key #'quit-window)
                  (use-local-map (make-composed-keymap
                                  map
                                  (current-local-map))))
                (save-excursion
                  (pcase content
                    ((pred stringp)
                     (insert content))
                    ((pred functionp)
                     (funcall content))
                    (_
                     (insert
                      (let ((print-level nil))
                        (funcall km-elisp-sexp-to-string-fn
                                 content))))))))
            (select-window window))))))

(defun km-elisp-make-file-link (file)
  "Return a buttonized or abbreviated FILE based on availability of `buttonize'.

Argument FILE is the path to the file to be linked."
  (let ((fn (if (fboundp 'buttonize) 'buttonize
              'button-buttonize)))
    (if (not (fboundp fn))
        (abbreviate-file-name file)
      (funcall fn
               (abbreviate-file-name file)
               #'find-file file))))

(defun km-elisp-make-feat-link (sym)
  "Create a clickable link for a feature symbol SYM that shows its dependents.

Argument SYM is the symbol representing the feature to link."
  (let ((fn (if (fboundp 'buttonize) 'buttonize
              'button-buttonize)))
    (if (not (fboundp fn))
        (format "%s" sym)
      (funcall fn
               (format "%s" sym)
               #'km-elisp-print-feature-dependents
               (format "%s" sym)))))

;;;###autoload
(defun km-elisp-print-feature-dependents (feat)
  "Display dependents and requirements of a specified feature in a popup buffer.

Argument FEAT is the name of the feature to analyze as a string."
  (interactive (list (completing-read "Feature: " features)))
  (require 'loadhist)
  (let* ((feat-file (feature-file (intern feat)))
         (feat-depends (and feat-file
                            (file-dependents
                             feat-file)))
         (feat-requires (and feat-file
                             (file-requires feat-file))))
    (km-elisp--popup "*km-elisp-depends*"
                     (lambda ()
                       (insert feat "\n\n")
                       (when feat-file
                         (insert "Source: "
                                 (km-elisp-make-file-link feat-file) "\n\n"))
                       (insert "Dependents: \n"
                               (mapconcat #'km-elisp-make-file-link feat-depends
                                          "\n")
                               "\n\n")
                       (insert "Requires:\n"
                               (mapconcat #'km-elisp-make-feat-link
                                          feat-requires "\n")))
                     #'special-mode)))

(provide 'km-elisp)
;;; km-elisp.el ends here
;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:
