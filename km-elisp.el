;;; km-elisp.el --- Miscellaneous Elisp utils -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-elisp
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "28.1"))
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


(defcustom km-elisp-use-package-symbol-names '("use-package"
                                               "use-package!")
  "List of symbol names for extra `use-package' declarations.

A list of symbol names that represent `use-package' macros or functions.

Each element in the list should be a string that corresponds to the name of a
`use-package' macro or function. These names are used to identify `use-package'
declarations when performing operations such as searching for package
declarations or copying their configurations.

The default value includes common variations of `use-package' declarations.
Users can add or remove entries to customize the behavior of functions that rely
on this list.

To modify this list, use `M-x `customize-option'` and search for the
corresponding customization option, then add or remove strings as needed. Ensure
that each entry is an exact match for the `use-package' macro or function name
used in the Emacs configuration."
  :group 'straight-extra
  :type '(repeat string))


(declare-function file-dependents "loadhist")
(declare-function file-requires "loadhist")
(declare-function find-library-name "find-func")
(declare-function lm-summary "lisp-mnt")
(declare-function lm-website "lisp-mnt")

(defcustom km-elisp-right-partial-functions '(fp-rpartial
                                              igist-rpartial
                                              js-imports--rpartial)
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
                                             fp-pipe fp-ignore-args
                                             igist-compose
                                             js-imports--compose
                                             js-imports--partial)
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

(define-widget 'km-elisp-menu-item-widget 'lazy
  "Define a widget for creating and customizing menu items with various properties.

Argument OFFSET is the number of spaces to indent the widget.

Argument TYPE specifies the type of the widget, which is a repeatable
choice of either a string or a vector.

Argument STRING is a simple string value.

Argument VECTOR is a vector containing a string, a radio button, and a set.

Argument RADIO is a radio button with a default value of `emacs-lisp-mode'.

Argument FUNCTION is a function to be executed.

Argument SEXP is a symbolic expression.

Argument SET is a set of inline lists.

Argument LIST is an inline list containing constants and values.

Argument CONST is a constant value.

Argument STYLE is a constant representing the style.

Argument TOGGLE is a constant representing a toggle option.

Argument ACTIVE is a constant representing the active state.

Argument SELECTED is a constant representing the selected state.

Argument LABEL is a constant representing the label.

Argument SUFFIX is a constant representing the suffix.

Argument VISIBLE is a constant representing the visibility.

Argument KEYS is a constant representing the keys.

Argument HELP is a constant representing the help text."
  :offset 4
  :type '(repeat
          (choice
           string
           (vector
            (string)
            (radio :value emacs-lisp-mode
             (function)
             (sexp))
            (set :inline t
             (list :inline t
              (const :style)
              (radio :value toggle
               (const toggle)
               (const radio)))
             (list :inline t
              (const :active)
              (sexp))
             (list
              :inline t
              (const :selected)
              (sexp))
             (list
              :inline t
              (const :label)
              (sexp))
             (list
              :inline t
              (const :suffix)
              (sexp))
             (list
              :inline t
              (const :visible)
              (sexp))
             (list
              :inline t
              (const :keys)
              (string))
             (list
              :inline t
              (const :help)
              (string))))
           km-elisp-menu-item-widget)))



(defcustom km-elisp-emacs-lisp-mode-menu '("Emacs-Lisp"
                                           ["Add File Local variable"
                                            add-file-local-variable]
                                           ["Add Directory Local variable"
                                            add-dir-local-variable]
                                           ["Elisp Scan" elisp-scan-menu]
                                           ["Jump to item" elmenu-jump :visible
                                            (featurep
                                             'elmenu-jump)]
                                           ["Show libraries that depend on file"
                                            km-elisp-print-feature-dependents]
                                           ["Find library " find-library]
                                           ["Load library " load-library]
                                           ["Unload Feature" unload-feature]
                                           ["Bundle items"
                                            elisp-bundle-include-undefined
                                            :visible (featurep 'elisp-bundle)]
                                           ["Generate org readme"
                                            org-autodoc-async]
                                           ["Indent Line" lisp-indent-line]
                                           ["Indent Region" indent-region
                                            :help
                                            "Indent each nonblank line in the region"
                                            :active mark-active]
                                           ["Comment Out Region" comment-region
                                            :help
                                            "Comment or uncomment each line in the region"
                                            :active mark-active]
                                           "---"
                                           ["Edebug" edebug-defun
                                            :help
                                            "Evaluate the top level form point is in, stepping through with Edebug"
                                            :keys "C-u C-M-x"]
                                           ("Compile"
                                            ["Byte-compile This File"
                                             emacs-lisp-byte-compile
                                             :help
                                             "Byte compile the file containing the current buffer"]
                                            ["Byte-compile and Load"
                                             emacs-lisp-byte-compile-and-load
                                             :help
                                             "Byte-compile the current file (if it has changed), then load compiled code"]
                                            ["Byte-recompile Directory..."
                                             byte-recompile-directory
                                             :help
                                             "Recompile every `.el' file in DIRECTORY that needs recompilation"]
                                            ["Disassemble Byte Compiled Object..."
                                             disassemble
                                             :help
                                             "Print disassembled code for OBJECT in a buffer"])
                                           ("Linting"
                                            ["Lint Defun" elint-defun
                                             :help "Lint the function at point"]
                                            ["Lint Buffer" elint-current-buffer
                                             :help "Lint the current buffer"]
                                            ["Lint File..." elint-file
                                             :help "Lint a file"]
                                            ["Lint Directory..." elint-directory
                                             :help "Lint a directory"])
                                           ("Profiling"
                                            ["Start Native Profiler..."
                                             profiler-start
                                             :help
                                             "Start recording profiling information"]
                                            ["Show Profiler Report"
                                             profiler-report
                                             :help
                                             "Show the current profiler report"
                                             :active (and (featurep 'profiler)
                                                      (profiler-running-p))]
                                            ["Stop Native Profiler"
                                             profiler-stop
                                             :help
                                             "Stop recording profiling information"
                                             :active (and (featurep 'profiler)
                                                      (profiler-running-p))]
                                            "---"
                                            ["Instrument Function..."
                                             elp-instrument-function
                                             :help
                                             "Instrument a function for profiling"]
                                            ["Instrument Package..."
                                             elp-instrument-package
                                             :help
                                             "Instrument for profiling all function that start with a prefix"]
                                            ["Show Profiling Results"
                                             elp-results
                                             :help
                                             "Display current profiling results"]
                                            ["Reset Counters for Function..."
                                             elp-reset-function
                                             :help
                                             "Reset the profiling information for a function"]
                                            ["Reset Counters for All Functions"
                                             elp-reset-all
                                             :help
                                             "Reset the profiling information for all functions being profiled"]
                                            "---"
                                            ["Remove Instrumentation for All Functions"
                                             elp-restore-all
                                             :help
                                             "Restore the original definitions of all functions being profiled"]
                                            ["Remove Instrumentation for Function..."
                                             elp-restore-function
                                             :help
                                             "Restore an instrumented function to its original definition"])
                                           ("Tracing"
                                            ["Trace Function..." trace-function
                                             :help
                                             "Trace the function given as an argument"]
                                            ["Trace Function Quietly..."
                                             trace-function-background
                                             :help
                                             "Trace the function with trace output going quietly to a buffer"]
                                            "---"
                                            ["Untrace All" untrace-all
                                             :help
                                             "Untrace all currently traced functions"]
                                            ["Untrace Function..."
                                             untrace-function
                                             :help
                                             "Untrace function, and possibly activate all remaining advice"])
                                           ["Construct Regexp" re-builder
                                            :help
                                            "Construct a regexp interactively"]
                                           ["Melpazoid"
                                            melpazoid-flymake-compile
                                            :visible
                                            (featurep 'melpazoid-flymake)]
                                           ["Flymake" flymenu-flymake :visible
                                            (featurep 'flymake-menu)]
                                           ["Check Documentation Strings"
                                            checkdoc
                                            :help
                                            "Check documentation strings for style requirements"]
                                           ["Auto-Display Documentation Strings"
                                            eldoc-mode
                                            :help
                                            "Display the documentation string for the item under cursor"
                                            :style toggle
                                            :selected (bound-and-true-p
                                                       eldoc-mode)]
                                           ["Autoformat message"
                                            autoformat-message-mode
                                            :style toggle
                                            :visible (featurep
                                                      'autoformat-message)
                                            :selected autoformat-message-mode
                                            :help
                                            "Inside message automatically add format string with %s escapes"]
                                           ["Long line mode" long-line-mode
                                            :style toggle
                                            :visible (featurep 'long-line)
                                            :selected long-line-mode
                                            :help
                                            "Show or hide fill column indicator after save"]
                                           ["Prettier elisp mode"
                                            prettier-elisp-mode
                                            :visible (fboundp 'prettier-mode)
                                            :style toggle
                                            :selected prettier-elisp-mode
                                            :help
                                            "Format current top level form on file save"]
                                           ["Prettier buffer"
                                            prettier-elisp-buffer :visible
                                            (fboundp
                                             'prettier-elisp-buffer)]
                                           ["Prettier function" prettier-elisp
                                            :visible (fboundp 'prettier-elisp)]
                                           ["Profiling" profiler-extra-menu
                                            :visible (fboundp
                                                      'profiler-extra-menu)]
                                           ["Explain pause mode"
                                            explain-pause-mode
                                            :style toggle
                                            :visible
                                            (fboundp 'explain-pause-mode)
                                            :selected (bound-and-true-p
                                                       explain-pause-mode)]
                                           ["Explain pause top"
                                            explain-pause-top
                                            :visible
                                            (fboundp 'explain-pause-top)
                                            :selected (bound-and-true-p
                                                       explain-pause-mode)]
                                           ["Show Startup results table"
                                            benchmark-init/show-durations-tabulated
                                            :visible
                                            (fboundp
                                             'benchmark-init/show-durations-tabulated)]
                                           ["Show Startup results tree"
                                            benchmark-init/show-durations-tree
                                            :visible
                                            (fboundp
                                             'benchmark-init/show-durations-tree)]
                                           ["Use package report"
                                            use-package-report
                                            :active
                                            use-package-compute-statistics]
                                           ["Esup" esup t]
                                           ["Bench buffer" bench-buffer t])
  "List of menu bar items to add in `emacs-lisp-mode-map'.

Usage:
\\=(easy-menu-define emacs-lisp-mode-menu emacs-lisp-mode-map
  \"Menu for Emacs Lisp mode.\" km-elisp-emacs-lisp-mode-menu)

Every item can be either
- string: as separator or submenu title,
- vector: a menu item [ NAME CALLBACK [ KEYWORD ARG ]... ]
- list of [STRING [MENU ITEM]... ]

A menu item may have the form:

   [ NAME CALLBACK [ KEYWORD ARG ]... ]

where NAME and CALLBACK have the same meanings as above, and each
optional KEYWORD and ARG pair should be one of the following:

 :keys KEYS
    KEYS is a string; a keyboard equivalent to the menu item.
    This is normally not needed because keyboard equivalents are
    usually computed automatically.  KEYS is expanded with
    `substitute-command-keys' before it is used.

 :key-sequence KEYS
    KEYS is a hint for speeding up Emacs's first display of the
    menu.  It should be nil if you know that the menu item has no
    keyboard equivalent; otherwise it should be a string or
    vector specifying a keyboard equivalent for the menu item.

 :active ENABLE
    ENABLE is an expression; the item is enabled for selection
    whenever this expression's value is non-nil.  `:enable' is an
    alias for `:active'.

 :visible INCLUDE
    INCLUDE is an expression; this item is only visible if this
    expression has a non-nil value.  `:included' is an alias for
    `:visible'.

 :label FORM
    FORM is an expression that is dynamically evaluated and whose
    value serves as the menu item's label (the default is NAME).

 :suffix FORM
    FORM is an expression that is dynamically evaluated and whose
    value is concatenated with the menu entry's label.

 :style STYLE
    STYLE is a symbol describing the type of menu item; it should
    be `toggle' (a checkbox), or `radio' (a radio button), or any
    other value (meaning an ordinary menu item).

 :selected SELECTED
    SELECTED is an expression; the checkbox or radio button is
    selected whenever the expression's value is non-nil.

 :help HELP
    HELP is a string, the help to display for the menu item.

Alternatively, a menu item can be a string.  Then that string
appears in the menu as unselectable text.  A string consisting
solely of dashes is displayed as a menu separator.

Alternatively, a menu item can be a list with the same format as
MENU."
  :group 'km-elisp
  :type 'km-elisp-menu-item-widget)


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

(defun km-elisp--inside-use-package-p-at-point ()
  "Check if point is on a `use-package' form."
  (or
   (when-let ((sexp (sexp-at-point)))
     (and
      (car-safe sexp)
      (symbolp (car-safe sexp))
      (member (symbol-name (car sexp)) km-elisp-use-package-symbol-names)
      (listp (cdr sexp))
      (symbolp (cadr sexp))
      (cadr sexp)))
   (when (looking-at (concat "(use-package[\s\t\n]+\\("
                             lisp-mode-symbol-regexp "\\)"))
     (match-string-no-properties 1))))

(defun km-elisp--ensure-beg-of-sexp ()
  "Move point to the beginning of the current s-expression."
  (pcase-let ((`(,beg . ,_end)
               (bounds-of-thing-at-point 'sexp)))
    (when beg
      (goto-char beg))))

(defun km-elisp--inside-use-package-cons-cell (&rest keywords)
  "Check if point is inside a `use-package' cons cell with specified keywords.

Argument KEYWORDS is a list of symbols to check against during the search."
  (km-elisp--ensure-beg-of-sexp)
  (when (looking-back "\\.[\s\t\n]*" 0)
    (let ((inside-keyword))
      (while (and (condition-case nil
                      (progn (backward-up-list)
                             (setq inside-keyword
                                   (progn (skip-chars-backward "\s\t\n")
                                          (unless (nth 4 (syntax-ppss (point)))
                                            (memq (symbol-at-point)
                                                  keywords))))
                             (not inside-keyword))
                    (error nil))))
      (and inside-keyword
           (condition-case nil
               (progn (backward-up-list)
                      (km-elisp--inside-use-package-p-at-point))
             (error nil))))))

;;;###autoload
(defun km-elisp-insert-random-string (&optional length)
  "Insert random string with length LENGTH."
  (interactive (list 12))
  (let ((chars (append (mapcar #'char-to-string
                               (number-sequence (string-to-char
                                                 "A")
                                                (string-to-char
                                                 "Z")))
                       (mapcar #'number-to-string (number-sequence 0 9)))))
    (let ((str))
      (dotimes (_i length)
        (setq str (concat str (elt chars (random (length chars))))))
      (insert (prin1-to-string str)))))


(defun km-elisp-extend-expect-function-p (pos)
  "Return non-nil if the symbol at position POS is expected to be a function.

POS is the position in the buffer to check.

This function extends the behavior of `elisp--expect-function-p' to include
additional macros specified in `km-elisp-expect-functions-ranges'.

\\=(advice-add \\='elisp--expect-function-p :after-until
              #\\='km-elisp-extend-expect-function-p)"
  (let ((parse-sexp-ignore-comments t))
    (or (save-excursion
          (goto-char pos)
          (km-elisp--inside-use-package-cons-cell :bind :bind*
                                                  :hook))
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
                (<= min-idx count max-idx))))))))

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

(defun km-elisp--indent-comments-backward ()
  "Indent comment lines backward according to the current mode."
  (indent-according-to-mode)
  (skip-chars-backward "\s\t")
  (while (looking-back "^[;]+[^;\n]+\n" 0)
    (forward-line -1)
    (indent-according-to-mode)
    (skip-chars-backward "\s\t")))

(defun km-elisp--get-comments-bounds-backward ()
  "Return the bounds of comment blocks preceding the current point."
  (let ((beg)
        (end (point)))
    (while (looking-back "^[\s]*[;]+[^;\n]+\n[\s]*" 0)
      (forward-line -1)
      (setq beg (point)))
    (and beg (cons beg end))))


;;;###autoload
(defun km-elisp-add-use-package-descriptions ()
  "Add summaries to `use-package' declarations in the current buffer."
  (interactive)
  (require 'find-func)
  (require 'lisp-mnt)
  (let ((regex
         (concat "(use-package[\s\t]+" "\\(" lisp-mode-symbol-regexp "\\)")))
    (save-excursion
      (goto-char (point-max))
      (let ((result))
        (while (re-search-backward regex nil t 1)
          (let ((name (match-string-no-properties 1)))
            (unless
                (let ((stx (syntax-ppss (point))))
                  (or (nth 3 stx)
                      (nth 4 stx)))
              (let* ((file (ignore-errors (find-library-name name)))
                     (summary (and file
                                   (when-let ((text (lm-summary file)))
                                     (concat
                                      ";; "
                                      (capitalize (substring-no-properties
                                                   text
                                                   0 1))
                                      (substring-no-properties text
                                                               1)))))
                     (url (and file
                               (when-let ((website (lm-website file)))
                                 (concat ";; " website))))
                     (description
                      (when (or
                             summary
                             url)
                        (string-join
                         (delq
                          nil
                          (list
                           summary
                           url))
                         "\n"))))
                (when description
                  (pcase-let* ((`(,beg . ,end)
                                (save-excursion
                                  (km-elisp--get-comments-bounds-backward)))
                               (curr-text
                                (when (and beg end)
                                  (buffer-substring-no-properties
                                   beg end)))
                               (lines (and curr-text
                                           (split-string curr-text "\n" t)))
                               (curr-summary (pop lines))
                               (curr-url (pop lines)))
                    (when curr-summary
                      (setq curr-summary (string-trim curr-summary)))
                    (when curr-url
                      (setq curr-url (string-trim curr-url)))
                    (cond ((and lines) nil)
                          ((and
                            url
                            (not curr-url)
                            curr-summary
                            summary
                            (string= curr-summary summary))
                           (insert (if (looking-back "\n[\s]*" 0) "" "\n")
                                   url "\n"))
                          ((and
                            curr-summary
                            summary
                            (string= (downcase curr-summary)
                                     (downcase summary)))
                           (delete-region beg end)
                           (insert (if (looking-back "\n[\s]*" 0) "" "\n")
                                   description
                                   "\n"))
                          (t (insert
                              (concat
                               (if (looking-back "\n[\s]*" 0) "" "\n")
                               description
                               "\n"))))))
                (km-elisp--indent-comments-backward)
                (push (cons name summary) result)))))
        result))))


(provide 'km-elisp)
;;; km-elisp.el ends here
;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:
