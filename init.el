(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin")
  (setq exec-path (append exec-path '(getenv "/usr/local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

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

(package-install 'flx-ido)
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(flx-ido-mode 1)
(ido-mode 1)
(setq ido-create-new-buffer 'always)

(package-install 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key [wheel-right] 'scroll-left)
(global-set-key [wheel-left] 'scroll-right)
(global-set-key [double-wheel-right] 'scroll-left)
(global-set-key [double-wheel-left] 'scroll-right)
(global-set-key [triple-wheel-right] 'scroll-left)
(global-set-key [triple-wheel-left] 'scroll-right)

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

(global-set-key (kbd "<f2>") 'rgrep)

(require 'linum-relative)

;; (package-install 'helm)
;; (require 'helm)
;; (require 'helm-config)

;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
;; (helm-autoresize-mode 1)

;; (helm-mode 1)

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

(windmove-default-keybindings 'super)
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
(global-set-key (kbd "s-f") 'jump-char-forward)
(global-set-key (kbd "s-F") 'jump-char-backward)

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

(require 'evil-magit)

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

(require 'evil)
(evil-mode 1)

;;  (defun my-move-key (keymap-from keymap-to key)
;;      "Moves key binding from one keymap to another, deleting from the old location. "
;;      (define-key keymap-to key (lookup-key keymap-from key))
;;      (define-key keymap-from key nil))
;; (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
;; (my-move-key evil-motion-state-map evil-normal-state-map " ")
(add-hook 'with-editor-mode-hook 'evil-insert-state)

(define-key evil-motion-state-map " " nil)
(define-key evil-motion-state-map "RET" nil)
(define-key evil-motion-state-map " u" 'universal-argument)
(define-key evil-motion-state-map " mgg" 'tern-find-definition)
(define-key evil-motion-state-map " m\C-g" 'tern-pop-find-definition)

(defun my-shell-mode ()
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))
(add-hook 'shell-mode-hook 'my-shell-mode)

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
 '(evil-want-C-u-scroll t)
 '(frame-background-mode (quote dark))
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(helm-completing-read-handlers-alist
   (quote
    ((rgrep . ido)
     (find-file . ido)
     (describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file)
     (execute-extended-command))))
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
 '(linum-relative-current-symbol "")
 '(linum-relative-global-mode t)
 '(magit-commit-arguments nil)
 '(neo-window-width 35)
 '(org-agenda-files "~/org/.agenda_files")
 '(package-selected-packages
   (quote
    (evil-magit linum-relative company flycheck tide multiple-cursors magit jump-char json-mode js2-mode company-tern avy)))
 '(require-final-newline t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(wdired-allow-to-change-permissions t)
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
