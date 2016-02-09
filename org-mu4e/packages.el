;;; packages.el --- org-mu4e layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Paul Schorfheide <pschorf2@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-mu4e-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-mu4e/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-mu4e/pre-init-PACKAGE' and/or
;;   `org-mu4e/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-mu4e-packages
  '(mu4e)
  "The list of Lisp packages required by the org-mu4e layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")
(defun org-mu4e/post-init-mu4e ()
  (require 'org-mu4e)
  (setq org-mu4e-link-query-in-headers-mode nil)
  (setq org-capture-templates '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
                                       "* TODO [#A] %?
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))
%a")))


  (setq mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder "/[Gmail].Sent Mail"
      mu4e-trash-folder "/[Gmail].Trash"
      mu4e-get-mail-command "offlineimap"
      user-mail-address "pschorf2@gmail.com"
      user-full-name "Paul Schorfheide")
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash" . ?t)
        ("/[Gmail].All Mail" . ?a))))
;;; packages.el ends here
