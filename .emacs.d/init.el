;; background color

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(background-color . "#000000"))

;; define function to add load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-to-loval-add-subdirs-to-load-path)
	    (normal-to-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; 改行とともにインデントを行う
(global-set-key (kbd "C-m") 'newline-and-indent)

;; C-c oでウィンドウを切り替える
(define-key global-map (kbd "C-c o") 'other-window)

;; 文字コード設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; テーマの設定
(when (require 'color-theme nil t)
  (color-theme-initalize)
  (color-theme-hober))

;; 括弧のハイライト
(setq show-paren-delay 0)
(show-paren-mode t)
(set-face-underline-p 'show-paren-match-face "yellow")

;; バックアップファイルを作成しない
(setq make-backup-files nil)

;; コード行数を表示
(global-linum-mode t)

;; ファイルが #! から始まる場合， +xをつけて保存する
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; auto-installの設定
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
;;  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; melpaの設定
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; auto-completeの設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; helmの設定
(require 'helm)
(require 'helm-config)
(require 'helm-ag)
(setq helm-ag-base-command "rg --vimgrep --no-heading")
(global-set-key (kbd "C-c s") 'helm-ag)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; multi-term
(when (require 'multi-term nil t)
  (setq multi-term-program "/usr/local/bin/zsh"))

(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
;; (set-default-font "Monaco 12")      ; Set font and size
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq tab-width 2)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

;; Add a directory to the load path so we can put extra files there
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers
  
  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Misc go stuff
  (auto-complete-mode 1))                         ; Enable auto-complete mode

;; flycheck golinter
(require 'flycheck)
(require 'flycheck-gometalinter)
(flycheck-gometalinter-setup) ;; flycheckのcheckerのリストにgometalinterを追加
(setq flycheck-gometalinter-fast t) ;; only run fast linters
(setq flycheck-gometalinter-test t) ;; use in tests files
(add-hook 'go-mode-hook 'flycheck-mode) ;; go-modeでflycheckを利用するように

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default))
  (add-hook 'go-mode-hook 'go-eldoc-setup)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

;; monokai-theme
(load-theme 'monokai t)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; python
(package-initialize)
(elpy-enable)

;; Bazel
(add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))

;; Protocol buffer
(require 'protobuf-mode)

;; Git
(require 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;; go-flymake
;;(add-to-list 'load-path "~/Go/src/github.com/dougm/goflymake")
;;   (require 'go-flymake)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flymake-json flycheck go-errcheck go-eldoc flycheck-gometalinter yaml-mode terraform-mode smartparens protobuf-mode projectile-ripgrep package-utils neotree multi-term monokai-theme helm-projectile helm-ag go-complete go-autocomplete git-gutter-fringe+ git-gutter flymake-go exec-path-from-shell elpy bazel-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
