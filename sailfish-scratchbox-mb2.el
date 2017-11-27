;;; sailfish-scratchbox-mb2.el --- Scratchbox building packages made simpler.

;; Copyright (C) 2017 Victor Polevoy

;; Author: V. V. Polevoy <fx@thefx.co>
;; Version: 1.0.0
;; Keywords: mb2, building, scratchbox, sailfish
;; URL: https://github.com/vityafx/mb2.el

;;; Commentary:

;; This packages provides easier way to run mb2 scripts as 'mb2 build' for example.

;;; Code:
(require 'compile)
;; (require 'f)


(defgroup scratchbox nil
  "Scratchbox utils"
  :group 'tools)

(defcustom scratchbox-interpreter "bash -ic"
  "The interpreter to run the scripts with."
  :type 'string
  :group 'scratchbox
  :safe #'stringp)

(defcustom scratchbox-which-sdk "sdk"
  "The path of the sdk environment script."
  :type 'string
  :group 'scratchbox
  :safe #'stringp)

(defcustom scratchbox-mb2-build "mb2 build"
  "The commad of the mb2 build script."
  :type 'string
  :group 'scratchbox
  :safe #'stringp)

(defcustom scratchbox-mb2-build-options ""
  "The mb2-build script options."
  :type 'string
  :group 'scratchbox
  :safe #'stringp)


(defun scratchbox-mb2-project-root ()
  "Return project root."
  (locate-dominating-file buffer-file-name ".git")
  )

(defun scratchbox-mb2-build-generate-command ()
  "Compiles a full cmd line for invoking mb2 build script.

Something like 'sdk mb2-build'"
  (concat scratchbox-interpreter " '" scratchbox-which-sdk " " scratchbox-mb2-build "'")
  )

(define-compilation-mode scratchbox-compilation-mode "scratchbox"
  "Scratchbox compilation mode")

(defun scratchbox-mb2-build-run ()
  "Run the mb2 build script on the project."
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))
  (when (get-buffer "*mb2 build*")
    (kill-buffer "*mb2 build*"))
  (let ((command-to-run (scratchbox-mb2-build-generate-command))
        (root-dir (scratchbox-mb2-project-root)))
    (with-current-buffer (get-buffer-create "*mb2 build*")
      (setq default-directory root-dir)
      (compilation-start command-to-run 'scratchbox-compilation-mode (lambda (m) (buffer-name))))))

;;;###autoload
(defun scratchbox-mb2-build ()
  "Test the project this file is in."
  (interactive)
  (scratchbox-mb2-build-run)
  )

(provide 'scratchbox)
;;; sailfish-scratchbox-mb2.el ends here
