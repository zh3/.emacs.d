(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(if (system-is-mac)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)))


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)

(global-set-key [wheel-right] 'scroll-left)
(global-set-key [wheel-left] 'scroll-right)
(global-set-key [double-wheel-right] 'scroll-left)
(global-set-key [double-wheel-left] 'scroll-right)
(global-set-key [triple-wheel-right] 'scroll-left)
(global-set-key [triple-wheel-left] 'scroll-right)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")

(add-to-list 'load-path "~/.emacs.d/web-mode")
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.[s]css$" . web-mode))
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(package-install 'js2-mode)
(defun my-js2-mode-hook ()
  (tern-mode t)
)
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(package-install 'flycheck)
(global-flycheck-mode)
; use local eslint for each file
; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (tern-mode t)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

(global-set-key (kbd "M-Z") (lambda (char) (interactive "cZap to char: ") (zap-to-char 1 char)))
(global-set-key (kbd "s-Z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)

(windmove-default-keybindings)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-default-notes-file "~/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(package-install 'jump-char)
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)

(package-install 'avy)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "C-:") 'avy-goto-word-0)
(setq avy-keys (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))

(package-install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(package-install 'magit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(package-install 'company)
(package-install 'company-tern)
(setq company-global-modes '(not org-mode))
(add-hook 'after-init-hook 'global-company-mode)
;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-tern))

(package-install 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background nil)
 '(company-backends
   (quote
    (company-tern company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(frame-background-mode (quote dark))
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-highlight-level 3)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-cond-assign-warning nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(js2-strict-var-hides-function-arg-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
 '(json-reformat:indent-width 2)
 '(neo-window-width 35)
 '(org-agenda-files "~/org/.agenda_files")
 '(require-final-newline t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(windmove-wrap-around t)
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "#553333" :foreground "highlightColor"))))
 '(ediff-current-diff-B ((t (:background "#335533" :foreground "highlightColor"))))
 '(ediff-current-diff-C ((t (:background "#888833" :foreground "highlightColor")))))

(load-theme 'solarized t)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
