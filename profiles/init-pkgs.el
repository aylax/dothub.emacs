;;; init-pkgs.el --- Support elisp manually installed in the pkgs dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Set load path

(require 'cl-lib)

(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Add both pkgs and its immediate subdirs to `load-path'
(let ((pkgs-dir (expand-file-name "pkgs/" user-emacs-directory)))
  (push pkgs-dir load-path)
  (sanityinc/add-subdirs-to-load-path pkgs-dir))

;;; Utilities for grabbing upstream libs

(defun pkgs-dir-for (name)
  (expand-file-name (format "pkgs/%s" name) user-emacs-directory))

(defun pkgs-library-el-path (name)
  (expand-file-name (format "%s.el" name) (pkgs-dir-for name)))

(defun download-pkgs-module (name url)
  (let ((dir (pkgs-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (pkgs-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (pkgs-library-loadable-p name)
    (byte-compile-file (download-pkgs-module name url))))

(defun pkgs-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/pkgs/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (pkgs-dir-for name)) f))))


(provide 'init-pkgs)
;;; init-pkgs.el ends here
