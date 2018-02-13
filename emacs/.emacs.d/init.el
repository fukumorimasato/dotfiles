;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  file: init.el
;;;  author: fukumori.masato
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;  directory isolation
;;;
(if load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  constant
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst hostname (downcase (car (split-string (system-name) "\\."))))
(defconst machine-linux (eq system-type 'gnu/linux) "Linux")
(defconst machine-mac (eq system-type 'darwin) "Mac")
(defconst machine-win (eq system-type 'darwin) "Windows")
(defconst running-24 (eq emacs-major-version 24) "running emacs 24.xx")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  proxy setting
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load *.el files under proxy-dir
(setq proxy-dir "proxy")
(dolist (dir (directory-files (locate-user-emacs-file ".") t))
  (when (string-match proxy-dir dir)  
    (dolist (el (directory-files dir t))
      (when (string-match "\\.el\\'" (file-name-nondirectory el))
	(load el t)
	)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  color theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  adapt solarized
(add-to-list 'custom-theme-load-path
	     (locate-user-emacs-file "themes/emacs-color-theme-solarized"))
(set-frame-parameter nil 'background-mode 'light) 
(set-terminal-parameter nil 'background-mode 'light) 
(load-theme 'solarized t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  el-get (package manager)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; install el-get (--> detect Not Found error. Should execute git clone manually)
;;;
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;;
;;;  install packages
;;;
(el-get-bundle ag)
(el-get-bundle company-mode)
(el-get-bundle company-irony)
(el-get-bundle emacs-async)
(el-get-bundle emacs-helm/helm :branch "v2.8.8") ;;need manualy "make"
(el-get-bundle swiper-helm)
(el-get-bundle jacktasia/dumb-jump :depends (f s dash popup))
(el-get-bundle powerline)
(el-get-bundle tom-tan/hlinum-mode)
(el-get-bundle k-talo/smooth-scroll
  :type git
  :url "https://github.com/k-talo/smooth-scroll.el")
;;(el-get-bundle flycheck/flycheck :depends (dash pkg-info let-alist cl-lib))
(el-get-bundle joaotavora/yasnippet)
(el-get-bundle yasnippet-snippets)
(el-get-bundle neotree)
(el-get-bundle find-file-in-project)
(el-get-bundle wolray/symbol-overlay  :depends (seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  setting for each package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  company-mode
;;;
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t) 
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)
;; yasnippetとの連携
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
	  (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;;
;;;  helm
;;;
(require 'helm-config)
(helm-mode +1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(setq helm-buffer-details-flag nil)
;;;
;;;  swiper
;;;
(global-set-key "\C-s" 'swiper)
(defvar swiper-include-line-number-in-search t) ;; line-numberでも検索可能
;;;
;;;  dumb-jump
;;;
(global-set-key (kbd "M-.") 'dumb-jump-go)
(global-set-key (kbd "M-*") 'dumb-jump-back)
(global-set-key (kbd "C-c C-j") 'dumb-jump-go)
(global-set-key (kbd "C-c C-n") 'dumb-jump-back)
(setq dumb-jump-force-searcher 'ag)
(dumb-jump-mode)
;;;
;;;  hlinum
;;;
(global-linum-mode t)
(set-face-foreground 'linum "gray64")
(setq linum-format "%5d ")
(require 'hl-line)
(set-face-background 'hl-line "black")
(set-face-attribute 'hl-line nil :inherit nil)
(require 'hlinum)
(hlinum-activate)
(set-face-foreground 'linum-highlight-face "#3FC")
(set-face-background 'linum-highlight-face "green")
;;;
;;;  flycheck
;;;
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;;
;;;  yasnippet
;;;
(yas-global-mode 1)  ;; enable yasnippet
;;;
;;;  neotree
;;;
;; F8でneotree-windowが開くようにする
(global-set-key [f8] 'neotree-toggle)
;;;
;;; smooth-scroll
;;;
(require 'smooth-scroll)
(smooth-scroll-mode t)
;;;
;;;  symbol-overlay
;;;
(require 'symbol-overlay)
(add-hook 'prog-mode-hook #'symbol-overlay-mode)
(add-hook 'markdown-mode-hook #'symbol-overlay-mode)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(define-key symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev) ;; 次のシンボルへ
(define-key symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next) ;; 前のシンボルへ
(define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all) ;; ハイライトキャンセル

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  misc setting
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(line-number-mode t)

;; goto-line
(global-set-key "\C-x\C-g" 'goto-line)
(put 'set-goal-column 'disabled nil)

;; 環境を日本語、UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; カーソル設定
(set-cursor-color "green")
(blink-cursor-mode t)

;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")

;; 列数を表示する
(column-number-mode t)

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; indent(default) => TAB (8文字幅)
(setq-default indent-tabs-mode t tab-width 8)

;; don't ask whether open symbolik linked file or not
(setq vc-follow-symlinks t)
