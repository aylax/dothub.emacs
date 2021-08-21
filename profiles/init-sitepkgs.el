;;; init-sitepkgs.el --- Support elisp manually installed in the sitepkgs dir -*- lexical-binding: t -*-
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

;; Add both sitepkgs and its immediate subdirs to `load-path'
(let ((sitepkgs-dir (expand-file-name "sitepkgs/" user-emacs-directory)))
  (push sitepkgs-dir load-path)
  (sanityinc/add-subdirs-to-load-path sitepkgs-dir))

;;; Utilities for grabbing upstream libs

(defun sitepkgs-dir-for (name)
  (expand-file-name (format "sitepkgs/%s" name) user-emacs-directory))

(defun sitepkgs-library-el-path (name)
  (expand-file-name (format "%s.el" name) (sitepkgs-dir-for name)))

(defun download-sitepkgs-module (name url)
  (let ((dir (sitepkgs-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (sitepkgs-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (sitepkgs-library-loadable-p name)
    (byte-compile-file (download-sitepkgs-module name url))))

(defun sitepkgs-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/sitepkgs/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (sitepkgs-dir-for name)) f))))


(provide 'init-sitepkgs)
;;; init-sitepkgs.el ends here
