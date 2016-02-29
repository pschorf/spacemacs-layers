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
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")))


  (setq mu4e-maildir "~/Maildir"
      mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder "/Sent Items"
      mu4e-trash-folder "/Deleted Items"
      mu4e-change-filenames-when-moving t
      mu4e-html2text-command "html2text -utf8 -width 72"
      mu4e-get-mail-command "mbsync twosigma"
      mu4e-update-interval 300
      mu4e-show-images t
      mu4e-use-fancy-chars t
      user-mail-address "pschorfh@twosigma.com"
      user-full-name "Paul Schorfheide")
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "msx.twosigma.com"
      smtpmail-smtp-server "msx.twosigma.com"
      smtpmail-smtp-service 587)
(setq mu4e-maildir-shortcuts
      '(("/" . ?i)
        ("/Sent Items" . ?s)
        ("/Deleted Items" . ?t)
        ("/Archive" . ?a)
        ("/Conversation History" . ?c)))
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-view-show-images t
      mu4e-html2text-command "html2text -b80 --unicode-snob --no-wrap-links"
      mu4e-view-image-max-width 800)

(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))
;;; packages.el ends here
