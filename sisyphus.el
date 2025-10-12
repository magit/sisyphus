;;; sisyphus.el --- Create releases of Emacs packages  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.sisyphus@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/sisyphus
;; Keywords: git tools vc

;; Package-Version: 0.3.0
;; Package-Requires: (
;;     (emacs   "30.1")
;;     (compat  "30.1")
;;     (cond-let "0.1")
;;     (elx      "2.3")
;;     (llama    "1.0")
;;     (magit    "4.4"))

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

    (transient-insert-suffix 'magit-tag "r"
      '("c" "release commit" sisyphus-create-release))

    (transient-suffix-put 'magit-tag "r" :description "release tag")

    (transient-append-suffix 'magit-tag "r"
      '("g" "post release commit"  sisyphus-bump-post-release))

    (transient-append-suffix 'magit-tag "g"
      '("y" "bump copyright years" sisyphus-bump-copyright))))

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

(defvar sisyphus-non-release-bump-header nil
  "Whether to bump the `Version'/`Package-Version' header for non-releases.

MELPA prefers release tags over the versions specified using the
`Version' or `Package-Version' library header, and for packages
that are only distributed on MELPA, I strongly recommend that you
do not add this header at all.

Elpa-Admin, the tool used to maintain GNU ELPA and NonGNU ELPA,
relies exclusively on the version header, making it mandatory to
set that header, if a package is to be distributed there.

The functions, which Emacs uses to compare version strings, only
support release and pre-release version strings.  Contrary to
what you might expect, they, for example, consider \"1.0-git\" to
be smaller than 1.0.  That makes it very difficult to bump the
version header after a release in order for it not to provide
incorrect information.

As it stands, we are left with two unsatisfactory options to deal
with the version header after a release.

1) We leave the header untouched until the next release.  This is
   very unfortunate because it means that every commit in between
   the releases N and N+1 will claim to actually be N.

   I regret having to knowingly provide incorrect information,
   but after years of refining the workaround that is the other
   option, I have given up, and now recommend you too just learn
   to live with this unfortunate situation as well.  (But again,
   if your package is not distributed on GNU ELPA or NonGNU ELPA,
   just avoid this situation by not adding this header at all.)

2) We bump the header in the first commit after a release by
   appending a suffix that is understood by Emacs and Elpa-Admin,
   and hopefully humans alike, as signifying a development
   snapshot that comes after the named release.

   Unfortunately, due to a series of blunders in how these tools
   work, this results in very ugly and long version strings.

   The suffix must contain something like \"-git\", because without
   such a pre-release suffix Elpa-Admin would mistake the \"this
   is not a release\" suffix for just another release.

   Because N-git is considered to be smaller than N, we have to
   additionally either inject a numeric part in between the release
   N and the suffix to fake post-release syntax, or bump N as well.

   a) Faking post-release syntax: Unlike N-git, N.50-git is greater
      than N.  Note that N.0-git would not work because N.0 is equal
      to N, so N.0-git too is smaller than N.

      Having to use such a noisy suffix is bad enough, but
      unfortunately Elpa-Admin then turns it into something even
      uglier.  \"1.2.3.50-git\", for example, becomes something like
      \"1.2.3.50-snapshot0.20230813.123456\".

      To use this approach you have to set two variables:

         (setq sisyphus-non-release-bump-header t)
         (setq sisyphus-non-release-suffix \".50-git\")

   b) Bumping version strings after release:  Given a release \"1.2.3\",
      bump the least significant part before appending a pre-release
      suffix: \"1.2.4-git\".

      It is important that you always bump the least significant part.
      If you bump another part, for example to \"1.3.0-git\", then you
      cannot later decide to release \"1.2.4\", without all snapshot
      releases that were previously labeled \"1.3.0-git...\" to be
      considered larger than the \"1.2.4\" release that follows them.

      To use this approach you have to set two variables:

         (setq sisyphus-non-release-bump-header t)
         (setq sisyphus-non-release-suffix \"-git\")

      Beside appending the specified suffix, this automatically bumps
      the least significant part of the version string of the
      preceding release.

A package may embed version strings in other places beside the
version header, in manuals, for example.  I recommend that you add a
post-release suffix to these version strings after a release.  Because
these version strings are not used by Emacs, we can ignore that Emacs
does not support post-release version strings.

Given the default values of `sisyphus-non-release-suffix' and this
variable, the command `sisyphus-bump-post-release' bumps all embedded
version strings by appending \"-devel\", except for the version in the
version library header, which it leaves untouched.")

(defvar sisyphus-non-release-suffix "-devel"
  "String appended to version strings for non-release revisions.
Depending on the value of `sisyphus-non-release-bump-header'
\(which see), this suffix is appended to all embedded version
strings, or to all except for the `Version'/`Package-Version'
header.")

(defvar sisyphus-changelog-file "CHANGELOG"
  "The file that contains the changelog.")

(defvar sisyphus-changelog-entry-regexp "^\\* v\\([^ ]+\\) +\\(.+\\)$"
  "Regexp used to match the beginning of an entry in a changelog file.

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
  "Format string used to insert changelog headings.")

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

;;; Commands

;;;###autoload
(defun sisyphus-create-release (version &optional nocommit)
  "Create a release commit, bumping version strings.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive (list (sisyphus--read-version)))
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t))
      (sisyphus--bump-changelog version)
      (sisyphus--bump-version version))
    (if nocommit
        (magit-refresh)
      (sisyphus--commit (format "Release version %s" version) t))))

;;;###autoload
(defun sisyphus-bump-post-release (version &optional nocommit)
  "Create a post-release commit, bumping version strings.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive (list (and (file-exists-p (expand-file-name "CHANGELOG"))
                          (sisyphus--read-version "Tentative next release"))
                     current-prefix-arg))
  (when (and sisyphus-non-release-bump-header
             (not (string-match-p "\\.[1-9][0-9]*-[a-z]*\\'"
                                  sisyphus-non-release-suffix)))
    (user-error "Configured `%s' cannot be used in header: %S"
                'sisyphus-non-release-suffix
                sisyphus-non-release-suffix))
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t))
      (sisyphus--bump-changelog version t)
      (sisyphus--bump-version (sisyphus--previous-version) t))
    (if nocommit
        (magit-refresh)
      (sisyphus--commit "Resume development"))))

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
      (sisyphus--commit "Bump copyright years" nil t))))

;;; Functions

(defun sisyphus--list-libs ()
  (seq-remove
   (##string-match-p "\\(\\`\\.\\|-autoloads\\.el\\'\\|-pkg.el\\'\\)"
                     (file-name-nondirectory %))
   (directory-files (if (file-directory-p "lisp") "lisp" ".") t "\\.el\\'")))

(defun sisyphus--list-orgs ()
  (seq-remove
   (##string-match-p "\\`\\(\\.\\|README.org\\'\\)"
                     (file-name-nondirectory %))
   (directory-files (if (file-directory-p "docs") "docs" ".") t "\\.org\\'")))

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
           (and (re-search-forward sisyphus-changelog-entry-regexp nil t)
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
        (if (re-search-forward sisyphus-changelog-entry-regexp nil t)
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

(defun sisyphus--bump-version (version &optional post-release)
  (let* ((libs (sisyphus--list-libs))
         (siblings (mapcar (##intern (file-name-base %)) libs))
         (version (if post-release
                      (concat version sisyphus-non-release-suffix)
                    version)))
    (dolist (lib libs)
      (sisyphus--bump-version-lib lib version siblings))
    (dolist (org (sisyphus--list-orgs))
      (sisyphus--bump-version-org org version))))

(defun sisyphus--bump-version-lib (file version siblings)
  (sisyphus--with-file file
    (when (lm-header "\\(Package-\\)?Version")
      (delete-region (point) (line-end-position))
      (insert (if (and (not sisyphus-non-release-bump-header)
                       (string-suffix-p sisyphus-non-release-suffix version))
                  (substring version 0 (- (length sisyphus-non-release-suffix)))
                version))
      (goto-char (point-min)))
    (when (re-search-forward
           (format "(defconst %s-version \"\\([^\"]+\\)\""
                   (file-name-base file))
           nil t)
      (replace-match version nil t nil 1)
      (goto-char (point-min)))
    (unless (string-suffix-p "-git" version)
      (sisyphus--bump-package-requires version siblings)
      (let ((prev (sisyphus--previous-version)))
        (while (re-search-forward
                ":package-version '([^ ]+ +\\. +\"\\([^\"]+\\)\")" nil t)
          (let ((found (match-str 1)))
            (when (and (magit--version> found prev)
                       (version< found version))
              (replace-match version nil t nil 1))))))))

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
  (dolist (lib (sisyphus--list-libs))
    (sisyphus--bump-copyright-lib lib))
  (when (sisyphus--list-orgs)
    (magit-call-process "make" "clean" "texi" "all")))

(defun sisyphus--bump-copyright-lib (file)
  (sisyphus--with-file file
    (let ((copyright-update t)
          (copyright-query nil))
      (copyright-update))))

(defun sisyphus--commit (msg &optional allow-empty no-edit)
  (setq magit--disable-save-buffers t)
  (let ((magit-inhibit-refresh t))
    (magit-stage-1 "-u"))
  (magit-commit-create
   (list "--edit" "--message" msg
         (and no-edit "--no-edit")
         (if (eq transient-current-command 'magit-tag)
             (and-let ((key (transient-arg-value
                             "--local-user=" (transient-args 'magit-tag))))
               (concat "--gpg-sign=" key))
           (transient-args 'magit-commit))
         (and allow-empty "--allow-empty"))))

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
                ((memq pkg '(emacs compat)))
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
;; read-symbol-shorthands: (
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; sisyphus.el ends here
