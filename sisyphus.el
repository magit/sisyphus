;;; sisyphus.el --- Create releases of Emacs packages  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.sisyphus@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/sisyphus
;; Keywords: git tools vc

;; Package-Version: 0.4.1
;; Package-Requires: (
;;     (emacs   "30.1")
;;     (compat  "31.0")
;;     (cond-let "1.1")
;;     (elx      "2.3")
;;     (llama    "1.0")
;;     (magit    "4.5"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Create a release and watch it roll down the hill again.

;;; Code:

(require 'compat)
(require 'cond-let)
(require 'llama)

(require 'copyright)
(require 'elx)
(require 'magit-tag)

(declare-function borg-worktree "ext:borg" (clone))

;;; Add Bindings

;;;###autoload
(defvar sisyphus-add-default-bindings t
  "Whether to add Sisyphus commands to the `magit-tag' menu.

If you want to disable that, you must set this to nil before
`magit-tag' is loaded (which happens when `magit' is loaded.")

;;;###autoload
(with-eval-after-load 'magit-tag
  (when sisyphus-add-default-bindings

    (transient-append-suffix 'magit-tag "r"
      '("e" "elisp release" sisyphus-create-release))

    (transient-append-suffix 'magit-tag "p"
      '("c" "bump copyright" sisyphus-bump-copyright))))

;;; Macros

(defmacro sisyphus--with-file (file &rest body)
  (declare (indent 1))
  (let ((file* (gensym "file"))
        (open* (gensym "open")))
    `(let* ((,file* ,file)
            (,open* (find-buffer-visiting ,file*)))
       (with-current-buffer (find-file-noselect ,file*)
         (save-excursion
           (goto-char (point-min))
           (prog1 (progn ,@body)
             (save-buffer)
             (unless ,open*
               (kill-buffer))))))))

;;; Variables

(defvar sisyphus-changelog-file "CHANGELOG"
  "The file that contains the changelog.")

(defvar sisyphus-changelog-heading-regexp "^\\* v\\([^ ]+\\) +\\(.+\\)$"
  "Regexp used to match a changelog heading.

The file is searched from the beginning.  I.e., it is assumed
that the first entry concerns either the unreleased development
version or the latest release.

The first match group must match the version string, without any
prefix such as \"v\".  The second match group must match a date
string.

The default value matches Org mode headings like these:

* v2.0.0    UNRELEASED

Description of upcoming changes.

* v1.0.0    2024/01/05

Description of changes in v1.0.0.

The regexp specified here, must match the format specified by
`sisyphus-changelog-heading-format'.")

(defvar sisyphus-changelog-heading-format "* v%-8v %d\n\n"
  "Format string used to insert changelog headings.
The format specified here, must match the regexp specified by
`sisyphus-changelog-heading-regexp'.")

(defvar sisyphus-bump-dependencies-function
  #'sisyphus-default-bump-dependencies
  "Function used to determine current versions of dependencies.

This function is called with three arguments DEPS, VERSION and SIBLINGS.
DEPS is the list of dependencies extracted from the Package-Requires
header and has the form ((DEP VER ALIGNMENT)...).  This function must
update each VER if appropriate, and return the updated list.  SIBLINGS
is a list of packages maintained in the same directory.  VERSION is
the version to be used for this package and its siblings.")

(defvar sisyphus-sort-dependencies-function
  #'sisyphus-default-sort-dependencies
  "Function used to sort dependencies.

This function is called with one argument ((NAME VERSION ALIGNMENT)...)
and must return a list of the same form.  The default function sorts
the dependencies alphabetically, except that \"emacs\" and \"compat\"
are placed before other dependencies.")

;;;###autoload(put 'sisyphus-libraries 'safe-local-variable #'listp)
(defvar-local sisyphus-libraries nil
  "List of libraries in which copyright years are bumped.
If nil, use the default set.  If t is a member of the list, also use
the default set.")

;;;###autoload(put 'sisyphus-org-manuals 'safe-local-variable #'listp)
(defvar-local sisyphus-org-manuals nil
  "List of manuals in Org format in which copyright years are bumped.
If nil, use the default set.  If t is a member of the list, also use
the default set.")

;;; Commands

;;;###autoload
(defun sisyphus-create-release (version &optional nocommit)
  "Create a release (commit and tag), bumping version strings.
With prefix argument NOCOMMIT, only bump version strings."
  (interactive (list (sisyphus--read-version)))
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t))
      (sisyphus--bump-changelog version)
      (sisyphus--bump-version version))
    (if nocommit
        (magit-refresh)
      (let ((date (format-time-string "%F %T %z")))
        (sisyphus--commit (format "Release version %s" version) date)
        (sisyphus--tag (format "v%s" version)
                       (format "%s %s"
                               (capitalize
                                (file-name-nondirectory
                                 (directory-file-name (magit-toplevel))))
                               version)
                       date)))))

;;;###autoload
(defun sisyphus-bump-package-requires ()
  "Bump versions in the visited library's Package-Requires header."
  (interactive)
  (magit-with-toplevel
    (let ((libs (sisyphus--list-libs)))
      (unless (member (expand-file-name buffer-file-name) libs)
        (user-error "Not visiting a library"))
      (sisyphus--bump-package-requires
       (sisyphus--previous-version)
       (mapcar (##intern (file-name-base %)) libs)))))

;;;###autoload
(defun sisyphus-bump-copyright (&optional nocommit)
  "Bump copyright years and commit the result.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive "P")
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t))
      (sisyphus--bump-copyright))
    (if nocommit
        (magit-refresh)
      (sisyphus--commit "Bump copyright years"))))

;;; Functions

(defun sisyphus--list-libs ()
  (let ((files (seq-remove
                (##string-match-p
                 "\\(\\`\\.\\|-autoloads\\.el\\'\\|-pkg.el\\'\\)"
                 (file-name-nondirectory %))
                (directory-files
                 (if (file-directory-p "lisp") "lisp" ".") t "\\.el\\'"))))
    (cond ((not sisyphus-libraries) files)
          ((memq t sisyphus-libraries)
           (delq t (nconc files sisyphus-libraries)))
          (sisyphus-libraries))))

(defun sisyphus--list-tests ()
  (cond-let
    ((file-directory-p "test")
     (directory-files "test" t "\\.el\\'"))
    [[file (format "%s-tests.el"
                   (file-name-nondirectory
                    (directory-file-name default-directory)))]]
    ((file-exists-p file)
     (list file))))

(defun sisyphus--list-orgs ()
  (let ((files (seq-remove
                (##string-match-p "\\`\\(\\.\\|README.org\\'\\)"
                                  (file-name-nondirectory %))
                (directory-files
                 (if (file-directory-p "docs") "docs" ".") t "\\.org\\'"))))
    (cond ((not sisyphus-org-manuals) files)
          ((memq t sisyphus-org-manuals)
           (delq t (nconc files sisyphus-org-manuals)))
          (sisyphus-org-manuals))))

(defun sisyphus--package-name ()
  (file-name-nondirectory (directory-file-name (magit-toplevel))))

(defun sisyphus--package-requires ()
  (save-excursion
    (let (deps beg end indent)
      (pcase (prog1 (lm-header "Package-Requires")
               (setq beg (point)))
        ('nil)
        ("("
         (forward-line 1)
         (while (looking-at "\
^;;\\(\s\\{3,\\}\\)(\\([^\s]+\\)\\([\s]+\\)\"\\([^\"]+\\)\")")
	   (push (list (intern (match-str 2)) (match-str 4) (match-str 3)) deps)
           (setq indent (match-str 1))
	   (forward-line 1))
         (setq deps (nreverse deps))
         (setq end (line-end-position 0)))
        (_
         (setq deps (read (current-buffer)))
         (setq end (point))))
      (and deps
           (list (lm--prepare-package-dependencies deps)
                 (1+ beg) (1- end) indent)))))

(defun sisyphus--previous-version ()
  (caar (magit--list-releases)))

(defun sisyphus--changelog-version ()
  (let ((file (expand-file-name sisyphus-changelog-file)))
    (and (file-exists-p file)
         (sisyphus--with-file file
           (and (re-search-forward sisyphus-changelog-heading-regexp nil t)
                (match-str 1))))))

(defun sisyphus--read-version (&optional prompt)
  (let* ((prev (sisyphus--previous-version))
         (next (sisyphus--changelog-version))
         (version (read-string
                   (if prev
                       (format "%s (previous was %s): "
                               (or prompt "Create release")
                               prev)
                     "Create first release: ")
                   (cond ((and next
                               (or (not prev)
                                   (magit--version> next prev)))
                          next)
                         (prev
                          (let ((v (version-to-list prev)))
                            (mapconcat #'number-to-string
                                       (nconc (butlast v)
                                              (list (1+ (car (last v)))))
                                       ".")))))))
    (when (and prev (not (magit--version> version prev)))
      (user-error "Version must increase, but %s is not greater than %s"
                  version prev))
    version))

(defun sisyphus--bump-changelog (version &optional stub)
  (let ((file (expand-file-name sisyphus-changelog-file))
        (err nil))
    (when (file-exists-p file)
      (sisyphus--with-file file
        (if (re-search-forward sisyphus-changelog-heading-regexp nil t)
            (let ((vers (match-str 1))
                  (date (match-str 2))
                  (prev (sisyphus--previous-version))
                  (today (format-time-string "%F")))
              (goto-char (line-beginning-position))
              (cond
                (stub
                 (sisyphus--bump-changelog-insert-heading version "UNRELEASED"))
                ((equal vers prev)
                 (sisyphus--bump-changelog-insert-heading version today)
                 (setq err "CHANGELOG entry missing; inserting stub"))
                ((equal vers version)
                 (when (and (not (equal date today))
                            (match-beginning 2))
                   (replace-match today nil t nil 2)))
                ((y-or-n-p
                  (format "%sCHANGELOG version is %s, change%s to %s?"
                          (if prev (format "Previous version is %s, " prev) "")
                          vers
                          (if prev " latter" "")
                          version))
                 (delete-region (point) (line-end-position))
                 (when (re-search-forward "\\=\n+" nil t)
                   (delete-region (match-beginning 0) (match-end 0)))
                 (sisyphus--bump-changelog-insert-heading version today))
                ((user-error "Abort"))))
          (user-error "Unsupported CHANGELOG format")))
      (when err
        (magit-refresh)
        (user-error err)))))

(defun sisyphus--bump-changelog-insert-heading (version date)
  (insert (format-spec sisyphus-changelog-heading-format
                       `((?v . ,version)
                         (?d . ,date)))))

(defun sisyphus--bump-version (version)
  (let* ((libs (sisyphus--list-libs))
         (siblings (mapcar (##intern (file-name-base %)) libs)))
    (dolist (lib libs)
      (sisyphus--bump-version-lib lib version siblings))
    (dolist (org (sisyphus--list-orgs))
      (sisyphus--bump-version-org org version))))

(defun sisyphus--bump-version-lib (file version siblings)
  (sisyphus--with-file file
    (when (lm-header "\\(Package-\\)?Version")
      (delete-region (point) (line-end-position))
      (insert version)
      (goto-char (point-min)))
    (when (re-search-forward
           (format "(defconst %s-version \"\\([^\"]+\\)\""
                   (file-name-base file))
           nil t)
      (replace-match version nil t nil 1)
      (goto-char (point-min)))
    (sisyphus--bump-package-requires version siblings)
    (let ((prev (sisyphus--previous-version)))
      (while (re-search-forward
              ":package-version '([^ ]+ +\\. +\"\\([^\"]+\\)\")" nil t)
        (let ((found (match-str 1)))
          (when (and (magit--version> found prev)
                     (version< found version))
            (replace-match version nil t nil 1)))))))

(defun sisyphus--bump-version-org (file version)
  (let ((modified nil))
    (sisyphus--with-file file
      (while (re-search-forward "{{{version(\\([^)]+\\))}}}" nil t)
        (replace-match version t t nil 1)
        (setq modified t))
      (unless modified
        (save-excursion
          (when (re-search-forward
                 "^#\\+subtitle: for version \\(.+\\)$" nil t)
            (replace-match version t t nil 1)
            (setq modified t))
          (when (re-search-forward
                 "^This manual is for [^ ]+ version \\(.+\\)\\.$" nil t)
            (replace-match version t t nil 1)
            (setq modified t)))))
    (when modified
      (magit-call-process "make" "texi"))))

(defun sisyphus--bump-package-requires (version siblings)
  (when-let ((deps (sisyphus--package-requires)))
    (pcase-let*
        ((`(,deps ,beg ,end ,indent) deps)
         (deps (funcall sisyphus-bump-dependencies-function
                        (funcall sisyphus-sort-dependencies-function deps)
                        version siblings)))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (if indent
            (pcase-dolist (`(,pkg ,version ,align) deps)
              (insert (format "\n;;%s(%s%s\"%s\")" indent pkg align version)))
          (insert (mapconcat (pcase-lambda (`(,pkg ,version))
                               (format "(%s \"%s\")" pkg version))
                             deps " ")))))))

(defun sisyphus--bump-copyright ()
  (dolist (file (nconc (sisyphus--list-libs)
                       (sisyphus--list-tests)))
    (sisyphus--bump-copyright-file file))
  (when (sisyphus--list-orgs)
    (let ((file (expand-file-name "docs/.orgconfig")))
      (when (file-exists-p file)
        (sisyphus--bump-copyright-file file)))
    (magit-call-process "make" "texi")))

(defun sisyphus--bump-copyright-file (file)
  (sisyphus--with-file file
    (let ((copyright-update t)
          (copyright-query nil))
      (copyright-update))))

(defun sisyphus--commit (msg &optional date)
  (setq magit--disable-save-buffers t)
  (let ((magit-inhibit-refresh t))
    (magit-stage-1 "-u"))
  (let ((git (magit-git-executable))
        (args (magit-process-git-arguments
               (list "commit" "--edit" "-m" msg (sisyphus--sign-argument))
               t))
        (date (or date (format-time-string "%F %T %z"))))
    (with-editor* "MAGIT_HOOK_EDITOR"
      (with-environment-variables
          ((magit-with-editor-envvar (getenv "MAGIT_HOOK_EDITOR")))
        (let ((magit-process-popup-time -1)
              (magit-inhibit-refresh t))
          (if (executable-find "datefudge")
              (apply #'magit-start-process "datefudge" nil
                     (nconc (list "--static" date git) args))
            (apply #'magit-start-process git nil args)))))))

(defun sisyphus--tag (tag msg date)
  (set-process-sentinel
   magit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (magit-process-sentinel process event))
     (when (eq (process-status process) 'exit)
       (let ((git (magit-git-executable))
             (args (magit-process-git-arguments
                    (list "tag" "-s" (sisyphus--sign-argument t) "-m" msg tag)
                    t)))
         (if (executable-find "datefudge")
             (apply #'magit-start-process "datefudge" nil
                    (nconc (list "--static" date git) args))
           (apply #'magit-start-process git nil args)))))))

(defun sisyphus--sign-argument (&optional tagging)
  (and-let* ((args (transient-args
                    (cond ((eq transient-current-command 'magit-commit)
                           '(magit-commit magit-tag))
                          ('(magit-tag magit-commit)))))
             (value (or (transient-arg-value "--gpg-sign=" args)
                        (transient-arg-value "--local-user=" args))))
    (concat (if tagging "--local-user=" "--gpg-sign=") value)))

(defun sisyphus-default-sort-dependencies (deps)
  (sort deps
        :lessp (lambda (a b)
                 (pcase (list a b)
                   (`(emacs  ,_) t)
                   (`(,_  emacs) nil)
                   (`(compat ,_) t)
                   (`(,_ compat) nil)
                   (_ (string< a b))))
        :key #'car))

(defun sisyphus-default-bump-dependencies (deps version siblings)
  (mapcar (pcase-lambda (`(,pkg ,ver ,align))
            (list pkg (if (memq pkg siblings) version ver) align))
          deps))

(defun sisyphus-tarsius-bump-dependencies (deps version siblings)
  (mapcar (pcase-lambda (`(,pkg ,ver ,align))
            (let* ((ver (version-to-list ver))
                   (parts (length ver)))
              (cond-let*
                ((eq pkg 'emacs))
                ((memq pkg siblings)
                 (setq ver (version-to-list version)))
                ([default-directory (borg-worktree (symbol-name pkg))]
                 [_(file-directory-p default-directory)]
                 (setq ver (version-to-list (sisyphus--previous-version))))
                ([builtin (alist-get pkg package--builtins)]
                 (setq ver (aref builtin 0))))
              (list pkg (package-version-join (seq-take ver parts)) align)))
          deps))

;;; _
(provide 'sisyphus)
;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-local-overrides: (
;;   (cond . 0)
;;   (interactive . 0))
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("thread$"      . "cond-let--thread$")
;;   ("when$"        . "cond-let--when$")
;;   ("and-let*"     . "cond-let--and-let*")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let*"      . "cond-let--if-let*")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let*"    . "cond-let--when-let*")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let*"   . "cond-let--while-let*")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; sisyphus.el ends here
