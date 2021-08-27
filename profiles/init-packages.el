;;; init-packages.el --- Support elisp manually installed in the packages dir -*- lexical-binding: t -*-
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

;; Add both packages and its immediate subdirs to `load-path'
(let ((packages-dir (expand-file-name "packages/" user-emacs-directory)))
  (push packages-dir load-path)
  (sanityinc/add-subdirs-to-load-path packages-dir))

;;; Utilities for grabbing upstream libs

(defun packages-dir-for (name)
  (expand-file-name (format "packages/%s" name) user-emacs-directory))

(defun packages-library-el-path (name)
  (expand-file-name (format "%s.el" name) (packages-dir-for name)))

(defun download-packages-module (name url)
  (let ((dir (packages-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (packages-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (packages-library-loadable-p name)
    (byte-compile-file (download-packages-module name url))))

(defun packages-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/packages/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (packages-dir-for name)) f))))


(provide 'init-packages)
;;; init-packages.el ends here
