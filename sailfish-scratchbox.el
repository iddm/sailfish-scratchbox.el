;;; sailfish-scratchbox.el --- Scratchbox building packages made simpler.

;; Copyright (C) 2017 Victor Polevoy

;; Author: V. V. Polevoy <fx@thefx.co>
;; Version: 1.0.1
;; Keywords: sb2, mb2, building, scratchbox, sailfish
;; URL: https://github.com/vityafx/mb2.el

;;; Commentary:

;; This packages provides easier way to run sailfish os sdk scripts
;; and tools as 'mb2 build' for example.

;;; Code:
(require 'compile)


(defgroup sailfish-scratchbox nil
  "Sailfish scratchbox utils"
  :group 'tools)

(defcustom sailfish-scratchbox-interpreter "bash -ic"
  "The interpreter to run the scripts with."
  :type 'string
  :group 'sailfish-scratchbox
  :safe #'stringp)

(defcustom sailfish-scratchbox-which-sdk "sdk"
  "The path of the sdk environment script."
  :type 'string
  :group 'sailfish-scratchbox
  :safe #'stringp)

(defcustom sailfish-scratchbox-mb2-build "mb2 build"
  "The command of the mb2 build script."
  :type 'string
  :group 'sailfish-scratchbox
  :safe #'stringp)

(defcustom sailfish-scratchbox-mb2-build-options ""
  "The mb2-build script options."
  :type 'string
  :group 'sailfish-scratchbox
  :safe #'stringp)


(defun scratchbox-project-root ()
  "Return project root."
  (or (locate-dominating-file buffer-file-name ".git/") (locate-dominating-file buffer-file-name "rpm/"))
  )

(defun scratchbox-mb2-build-generate-command ()
  "Compile a full cmd line for invoking mb2 build script.

Something like 'sdk mb2-build'"
  (concat sailfish-scratchbox-interpreter " '" sailfish-scratchbox-which-sdk " "
          sailfish-scratchbox-mb2-build  " " sailfish-scratchbox-mb2-build-options "'")
  )

(define-compilation-mode sailfish-scratchbox-compilation-mode "sailfish scratchbox"
  "Sailfish scratchbox compilation mode")

(defun scratchbox-mb2-build-run ()
  "Run the mb2 build script on the project."
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))
  (when (get-buffer "*scratchbox build*")
    (kill-buffer "*scratchbox build*"))
  (let ((command-to-run (scratchbox-mb2-build-generate-command))
        (root-dir (scratchbox-project-root)))
    (with-current-buffer (get-buffer-create "*scratchbox build*")
      (setq default-directory root-dir)
      (compilation-start command-to-run 'sailfish-scratchbox-compilation-mode (lambda (m) (buffer-name))))))

;;;###autoload
(defun sailfish-scratchbox-mb2-build ()
  "Test the project this file is in."
  (interactive)
  (scratchbox-mb2-build-run)
  )

(provide 'sailfish-scratchbox)
;;; sailfish-scratchbox.el ends here
