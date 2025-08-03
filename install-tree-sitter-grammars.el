;;; install-tree-sitter-grammars.el --- Install and compress tree-sitter grammars -*- lexical-binding: t; -*-

;;; Commentary:
;; This script installs all configured tree-sitter grammars and compresses them
;; by stripping debug symbols.

;;; Code:

(require 'treesit)

(defun my/compress-tree-sitter-grammars ()
  "Compress installed tree-sitter grammars by stripping debug symbols."
  (interactive)
  (let* ((grammar-dir (expand-file-name "tree-sitter" user-emacs-directory))
         (libs (directory-files grammar-dir t "\\.so$")))
    (if libs
        (progn
          (message "Found %d tree-sitter libraries to compress..." (length libs))
          (dolist (lib libs)
            (message "Stripping %s..." (file-name-nondirectory lib))
            (call-process "strip" nil nil nil "--strip-all" lib))
          (message "All tree-sitter grammars have been compressed!"))
      (message "No tree-sitter libraries found in %s" grammar-dir))))

(defun my/install-all-tree-sitter-grammars ()
  "Install all configured tree-sitter grammars and compress them."
  (interactive)
  (let ((languages (mapcar #'car treesit-language-source-alist))
        (installed 0)
        (failed 0))
    (message "Installing %d tree-sitter grammars..." (length languages))
    (dolist (lang languages)
      (condition-case err
          (progn
            (message "Installing tree-sitter grammar for %s..." lang)
            (treesit-install-language-grammar lang)
            (setq installed (1+ installed)))
        (error
         (message "Failed to install %s: %s" lang (error-message-string err))
         (setq failed (1+ failed)))))
    (message "Installation complete: %d installed, %d failed" installed failed)
    (when (> installed 0)
      (message "Compressing installed grammars...")
      (my/compress-tree-sitter-grammars))))

(defun my/check-tree-sitter-installation ()
  "Check the status of tree-sitter grammar installations."
  (interactive)
  (let ((languages (mapcar #'car treesit-language-source-alist))
        (installed '())
        (missing '()))
    (dolist (lang languages)
      (if (treesit-language-available-p lang)
          (push lang installed)
        (push lang missing)))
    (message "Tree-sitter grammar status:")
    (message "  Installed (%d): %s" (length installed) 
             (mapconcat #'symbol-name (nreverse installed) ", "))
    (message "  Missing (%d): %s" (length missing)
             (mapconcat #'symbol-name (nreverse missing) ", "))
    (when missing
      (when (yes-or-no-p "Install missing grammars? ")
        (dolist (lang missing)
          (condition-case err
              (progn
                (message "Installing %s..." lang)
                (treesit-install-language-grammar lang))
            (error
             (message "Failed to install %s: %s" lang (error-message-string err)))))
        (my/compress-tree-sitter-grammars)))))

;; Add convenient key bindings when this file is loaded
(global-set-key (kbd "C-c t i") #'my/install-all-tree-sitter-grammars)
(global-set-key (kbd "C-c t c") #'my/check-tree-sitter-installation)
(global-set-key (kbd "C-c t s") #'my/compress-tree-sitter-grammars)

(provide 'install-tree-sitter-grammars)
;;; install-tree-sitter-grammars.el ends here