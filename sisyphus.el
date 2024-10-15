;;; sisyphus.el --- Create releases of Emacs packages  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.sisyphus@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/sisyphus
;; Keywords: git tools vc

;; Package-Version: 0.2.0
;; Package-Requires: (
;;     (emacs "27.1")
;;     (compat "30.0.0.0")
;;     (elx "2.0.3")
;;     (llama "0.3.1")
;;     (magit "4.1.1"))

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
(require 'llama)

(require 'copyright)
(require 'elx)
(require 'magit-tag)

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

;;; Variables

(defvar sisyphus-non-release-bump-header nil
  "Whether to bump the `Package-Version' header for non-releases.

MELPA prefers release tags over the `Package-Version' library
header, and for packages that are only distributed on MELPA, I
strongly recommend that you do not add this header at all.

Elpa-Admin, the tool used to maintain GNU ELPA and NonGNU ELPA,
relies exclusively on the `Package-Version' header, making it
mandatory to set that header if a package is to be distributed
there.

The functions, which Emacs uses to compare version strings, only
support release and pre-release version strings.  Contrary to
what you might expect, they, for example, consider \"1.0-git\" to
be smaller than 1.0.  That makes it very difficult to bump the
`Package-Version' after a release in order for it not to provide
incorrect information.

As it stands, we are left with two unsatisfactory options to deal
with the `Package-Version' header after a release.

1) We leave the header untouched until the next release.  This is
   very unfortunate because it means that every commit inbetween
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
   additionally inject a numeric part inbetween the release N and
   the suffix.  Unlike N-git, N.50-git is greater than N.  Note
   that N.0-git would not work because N.0 is equal to N, so
   N.0-git too is smaller than N.

   Having to use such a noisy suffix is bad enough, but
   unfortunately Elpa-Admin then turns it into something even
   uglier.  \"1.2.3.50-git\", for example, becomes something like
   \"1.2.3.50-snapshot0.20230813.123456\".

A package may embbed version strings in other places beside the
`Package-Version', in manuals, for example.  I recommend that
you add a post-release suffix to these version strings after a
release.  Because these version strings are not used by Emacs,
we can ignore that Emacs does not support post-release version
strings.

The default suffix, specified by `sisyphus-non-release-suffix',
is \"-devel\", which was chosen over, e.g., \"-git\", because Emacs
does not recognize the former at all, while it would considers the
latter to be a pre-release suffix.

Given the default values of `sisyphus-non-release-suffix' and
this variable, the command `sisyphus-bump-post-release' bumps
all embedded version strings by appending \"-devel\", except for
the `Package-Version' header, which it leaves untouched.")

(defvar sisyphus-non-release-suffix "-devel"
  "String appended to version strings for non-release revisions.
Depending on the value of `sisyphus-non-release-bump-header'
\(which see), this suffix is appended to all embedded version
strings, or to all except for the `Package-Version' header.")

;;; Commands

;;;###autoload
(defun sisyphus-create-release (version &optional nocommit)
  "Create a release commit, bumping version strings.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive (list (sisyphus--read-version)))
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t)
          (magit--disable-save-buffers t))
      (sisyphus--bump-changelog version)
      (sisyphus--bump-version version))
    (unless nocommit
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
    (let ((magit-inhibit-refresh t)
          (magit--disable-save-buffers t))
      (sisyphus--bump-changelog version t)
      (sisyphus--bump-version (sisyphus--previous-version) t))
    (unless nocommit
      (sisyphus--commit "Resume development"))))

;;;###autoload
(defun sisyphus-bump-copyright (&optional nocommit)
  "Bump copyright years and commit the result.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive "P")
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t)
          (magit--disable-save-buffers t))
      (sisyphus--bump-copyright))
    (unless nocommit
      (sisyphus--commit "Bump copyright years" nil t))))

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

;;; Functions

(defun sisyphus--package-name ()
  (file-name-nondirectory (directory-file-name (magit-toplevel))))

(defun sisyphus--previous-version ()
  (caar (magit--list-releases)))

(defun sisyphus--get-changelog-version ()
  (let ((file (expand-file-name "CHANGELOG")))
    (and (file-exists-p file)
         (sisyphus--with-file file
           (and (re-search-forward "^\\* v\\([^ ]+\\)" nil t)
                (match-string-no-properties 1))))))

(defun sisyphus--read-version (&optional prompt)
  (let* ((prev (sisyphus--previous-version))
         (next (sisyphus--get-changelog-version))
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
  (let ((file (expand-file-name "CHANGELOG")))
    (when (file-exists-p file)
      (sisyphus--with-file file
        (if (re-search-forward "^\\* v\\([^ ]+\\) +\\(.+\\)$" nil t)
            (let ((vers (match-string-no-properties 1))
                  (date (match-string-no-properties 2))
                  (prev (sisyphus--previous-version))
                  (today (format-time-string "%F")))
              (goto-char (line-beginning-position))
              (cond
               (stub
                (insert (format "* v%-9sUNRELEASED\n\n" version)))
               ((equal vers prev)
                (insert (format "* v%-9s%s\n\n" version today))
                (user-error "CHANGELOG entry missing; inserting stub"))
               ((equal vers version)
                (unless (equal date today)
                  (replace-match today nil t nil 2)))
               ((y-or-n-p
                 (format "%sCHANGELOG version is %s, change%s to %s"
                         (if prev (format "Previous version is %s, " prev) "")
                         vers
                         (if prev " latter" "")
                         version))
                (delete-region (point) (line-end-position))
                (insert (format "* v%-9s%s" version today)))
               ((user-error "Abort"))))
          (user-error "Unsupported CHANGELOG format"))))))

(defun sisyphus--list-files ()
  (let* ((lisp (if (file-directory-p "lisp") "lisp" "."))
         (docs (if (file-directory-p "docs") "docs" "."))
         (pkgs (nconc (directory-files lisp t "-pkg\\.el\\'")
                      (and (equal lisp "lisp")
                           (directory-files "." t "-pkg\\.el\\'"))))
         (libs (cl-set-difference
                (directory-files lisp t "\\.el\\'")
                (nconc (directory-files lisp t "-autoloads\\.el\\'")
                       pkgs)
                :test #'equal))
         (orgs (cl-delete "README.org" (directory-files docs t "\\.org\\'")
                          :test #'equal :key #'file-name-nondirectory)))
    (list libs pkgs orgs)))

(defun sisyphus--bump-version (release &optional post-release)
  (pcase-let*
      ((`(,libs ,pkgs ,orgs) (sisyphus--list-files))
       (version (if post-release
                    (concat release sisyphus-non-release-suffix)
                  release))
       (lib-updates (mapcar (lambda (lib)
                              (list (intern
                                     (file-name-sans-extension
                                      (file-name-nondirectory lib)))
                                    version))
                            libs))
       (pkg-updates (if post-release
                        (let ((timestamp (format-time-string "%Y%m%d")))
                          (mapcar (pcase-lambda (`(,pkg ,_)) (list pkg timestamp))
                                  lib-updates))
                      lib-updates)))
    (mapc (##sisyphus--bump-version-pkg % version pkg-updates) pkgs)
    (mapc (##sisyphus--bump-version-lib % version release lib-updates) libs)
    (mapc (##sisyphus--bump-version-org % version) orgs)))

(defun sisyphus--bump-version-pkg (file version updates)
  (sisyphus--with-file file
    (pcase-let* ((`(,_ ,name ,_ ,docstring ,deps . ,props)
                  (read (current-buffer)))
                 (deps (cadr deps)))
      (erase-buffer)
      (insert (format "(define-package %S %S\n  %S\n  '("
                      name version docstring))
      (when deps
        (setq deps (elx--update-dependencies deps updates))
        (let ((format
               (format "(%%-%is %%S)"
                       (apply #'max
                              (mapcar (##length (symbol-name (car %))) deps)))))
          (while-let ((dep (pop deps)))
            (indent-to 4)
            (insert (format format (car dep) (cadr dep)))
            (when deps (insert "\n")))))
      (insert ")")
      (while-let ((key (pop props))
                  (val (pop props)))
        (insert (format "\n  %s %S" key val)))
      (insert ")\n"))))

(defun sisyphus--bump-version-lib (file version release updates)
  (sisyphus--with-file file
    (when (lm-header "Package-Version")
      (delete-region (point) (line-end-position))
      ;; If we are creating a release, then `version' and `release'
      ;; are the same, so then this conditional makes no difference.
      (insert (if sisyphus-non-release-bump-header version release))
      (goto-char (point-min)))
    (when (re-search-forward
           (format "(defconst %s-version \"\\([^\"]+\\)\""
                   (file-name-sans-extension
                    (file-name-nondirectory file)))
           nil t)
      (replace-match version nil t nil 1)
      (goto-char (point-min)))
    (unless (string-suffix-p "-git" version)
      (elx-update-package-requires nil updates nil t)
      (let ((prev (sisyphus--previous-version)))
        (while (re-search-forward
                ":package-version '([^ ]+ +\\. +\"\\([^\"]+\\)\")" nil t)
          (let ((found (match-string-no-properties 1)))
            (when (and (magit--version> found prev)
                       (version< found version))
              (replace-match version nil t nil 1))))))
    (save-buffer)))

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

(defun sisyphus--bump-copyright ()
  (pcase-let ((`(,libs ,_ ,orgs) (sisyphus--list-files)))
    (mapc (##sisyphus--bump-copyright-lib %) libs)
    (when orgs
      (magit-call-process "make" "clean" "texi" "all"))))

(defun sisyphus--bump-copyright-lib (file)
  (sisyphus--with-file file
    (let ((copyright-update t)
          (copyright-query nil))
      (copyright-update))))

(defun sisyphus--commit (msg &optional allow-empty no-edit)
  (let ((magit-inhibit-refresh t))
    (magit-stage-1 "-u"))
  (magit-commit-create
   (list "--edit" "--message" msg
         (and no-edit "--no-edit")
         (if (eq transient-current-command 'magit-tag)
             (and-let* ((key (transient-arg-value
                              "--local-user=" (transient-args 'magit-tag))))
               (concat "--gpg-sign=" key))
           (transient-args 'magit-commit))
         (and allow-empty "--allow-empty"))))

;;; _
(provide 'sisyphus)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; sisyphus.el ends here
