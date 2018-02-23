;;; init.el -- fukumori's emacs setting file

;; Filename: init.el
;; Package-Requires:
;; author: fukumori.masato.dev@gmail.com
;; version:
;; URL:

;;; commentary:

;;; code:

(package-initialize)

;;;
;;;  directory isolation
;;;

(if load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;;
;;;  environment variable
;;;
(defconst my-hostname (downcase (car (split-string (system-name) "\\."))))
(defconst machine-linux (eq system-type 'gnu/linux) "Linux.")
(defconst machine-mac (eq system-type 'darwin) "Mac.")
(defconst machine-win (eq system-type 'darwin) "Windows.")
(defconst running-24 (eq emacs-major-version 24) "Running Emacs 24.")

;;;
;;;  proxy setting
;;;
;; load *.el files under proxy-dir
(defconst proxy-file "proxy/myproxy.el")
(load (locate-user-emacs-file proxy-file) t)


;;;
;;;  Setting for package.el
;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t) ;; didn't work.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;;
;;;  Setting for use-package
;;;
;;;;; ensure to use use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;
;;;  color theme
;;;
;;  adapt solarized
(use-package solarized-theme
  :init
  (add-to-list 'custom-theme-load-path
	       (locate-user-emacs-file "themes/emacs-color-theme-solarized"))
  (defun my-theme-setup-hook ()
    (interactive)
    (set-terminal-parameter nil 'background-mode 'light)
    (set-frame-parameter nil 'background-mode 'light)
    (load-theme 'solarized t)
    )
  (my-theme-setup-hook)
  (add-hook 'tty-setup-hook 'my-theme-setup-hook)
  )

;;;
;;;  package install by use-package and el-get
;;;

(use-package key-chord          :ensure t :config (key-chord-mode 1))
(use-package use-package-chords :ensure t)

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


;;
;;  dsvn
;;
(use-package dsvn
  :disabled t ;; not comfortable for me.
  :ensure t
  )

;;
;; ag
;;
(use-package ag
  :init
  (el-get-bundle ag)
  )

;;;
;;; migemo
;;;
;; referenced url: https://github.com/tsu-nera/dotfiles/blob/master/.emacs.d/inits/20_text.org
(use-package migemo
  :ensure t
  :config
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  (when machine-linux
    ;; ubuntu 16.04 LTS apt-get でインストールした場合
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))

  ;;  (when windows-p
  ;;    (setq migemo-command "c:/app/cmigemo-default-win64/cmigemo.exe")
  ;;    (setq migemo-dictionary "c:/app/cmigemo-default-win64/dict/utf-8/migemo-dict"))
  (migemo-init)
  )

;;;
;;; helm
;;;
(use-package helm
  :init
  (el-get-bundle emacs-helm/helm :branch "v2.8.8") ;;need manualy "make"
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("M-y"     . helm-show-kill-ring)
   ("C-c i"   . helm-imenu)
   ("C-x C-b" . nil)
   ("C-x C-b" . helm-buffers-list)
   :map helm-map
   ("C-h" . delete-backward-char)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("TAB" . helm-execute-persistent-action)
   :map helm-read-file-map
   ("TAB" . helm-execute-persistent-action)
   )
  :chords
  
  :config
  (setq helm-buffer-details-flag nil)
  (recentf-mode 1)
  (helm-mode 1)
  )

;;
;; helm-projectile/projectile
;;
(use-package helm-projectile
  :init
  (el-get-bundle helm-projectile :depends (projectile))
  (projectile-mode)
  (helm-projectile-on)
  )

;;;
;;; helm-swoop
;;;
(use-package helm-swoop
  :ensure t
  :bind
  ("C-s" . helm-swoop)
  )

;;;
;;; swoop
;;;
(use-package swoop
  :ensure t
  :init
  (defvar swoop-migemo-options "-q -e -d /usr/share/cmigemo/utf-8/migemo-dict")
  )

;;;
;;;  swiper-helm
;;;
(use-package swiper-helm
  :disabled t  ;; not comfortable for me.
  :init
  (el-get-bundle swiper-helm)
  :bind
  ("\C-s" . swiper-helm)
  :config
  (defvar swiper-include-line-number-in-search t) ;; line-numberでも検索可能
  )

;;;
;;;  helm-descbinds
;;;
(use-package helm-descbinds
  :init
  (el-get-bundle helm-descbinds)
  :config
  (helm-descbinds-mode 1)
  )

;;;
;;;  dumb-jump
;;;
(use-package dumb-jump
  :init
  (el-get-bundle jacktasia/dumb-jump :depends (f s dash popup))
  :bind
  ("M-." . dumb-jump-go)
  ("M-*" . dumb-jump-back)
  ("C-c C-j" . dumb-jump-go)
  ("C-c C-n" . dumb-jump-back)
  :config
  ;;(setq dumb-jump-selector 'helm)  ;; "Invalid function: helm-build-sync-source" occured
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'ag)
  (dumb-jump-mode)
  )


;;;
;;;  yasnippet: enable intertion of snippet.
;;;
(use-package yasnippet
  :init
  (el-get-bundle joaotavora/yasnippet)
  (el-get-bundle helm-c-yasnippet)
  (el-get-bundle yasnippet-snippets)

  :bind
  (:map yas-minor-mode-map
;;   ("C-x &" . nil)
;;   ("C-x & C-n" . nil)
;;   ("C-x & C-s" . nil)
;;   ("C-x & C-v" . nil)
   ("C-x i n" . yas-new-snippet)
   ("C-x i i" . yas-insert-snippet)
   ("C-x i v" . yas-visit-snippet-file)
   ("C-c y" . helm-yas-complete)
   )

  :config
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages
     (quote
      (helm el-get use-package-chords key-chord use-package)))
   '(yas-trigger-key "TAB"))
  (setq helm-c-yas-space-match-any-greedy t)
  (yas-global-mode 1)  ;; enable yasnippet
  )

;;;
;;;  company
;;;
;; referenced url:
;;  https://qiita.com/sune2/items/b73037f9e85962f5afb7
;;  https://qiita.com/sune2/items/c040171a0e377e1400f6
;;
(use-package company
  :defer t
  :init
  (el-get-bundle company-mode)
  (add-hook 'after-init-hook 'global-company-mode)

  :bind
  (:map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-h" . nil)
   )
  
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  ;; yasnippetとの連携
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (defun set-yas-as-company-backend ()
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    )
  (add-hook 'company-mode-hook 'set-yas-as-company-backend)  
  )

(use-package company-irony
  :disabled t  ;; Can not install irony server on Ubuntu 16.04
  :init
  (el-get-bundle irony-mode) 
  (el-get-bundle company-irony)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (add-to-list 'company-backends 'company-irony) ; backend追加  
  )

;;;
;;; emacs-async
;;;
(use-package async
  :init
  (el-get-bundle emacs-async)
  )

;;;
;;; powerline
;;;
(use-package powerline
  :init
  (el-get-bundle powerline)
  )

;;;
;;; hlinum: 左側に行を表示する.
;;;
(el-get-bundle tom-tan/hlinum-mode)
(global-linum-mode t)
(set-face-foreground 'linum "gray64")
(setq linum-format "%5d ")
(require 'hl-line)
;;(set-face-background 'hl-line "black")
(set-face-attribute 'hl-line nil :inherit nil)
(global-hl-line-mode)
(require 'hlinum)
(hlinum-activate)
(set-face-foreground 'linum-highlight-face "#3FC")
(set-face-background 'linum-highlight-face "green")

;;;
;;; smooth-scroll
;;;
(use-package smooth-scroll
  :init
  (el-get-bundle k-talo/smooth-scroll
    :type git
    :url "https://github.com/k-talo/smooth-scroll.el")
  
  :config
  (smooth-scroll-mode)
  )


;;;
;;;  flycheck: syntacks check.
;;;
(el-get-bundle flycheck)
(el-get-bundle yasuyk/helm-flycheck)
(global-set-key (kbd "C-c !") 'nil)
(global-set-key (kbd "C-c C-c") 'helm-mode-flycheck-compile)
(global-set-key (kbd "C-c C-n") 'flycheck-next-errors)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-errors)
(global-set-key (kbd "C-c C-l") 'flycheck-list-errors)
(global-set-key (kbd "C-c C-f") 'helm-flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;
;;;  neotree: Display directory tree.
;;;
(use-package neotree
  :init
  (el-get-bundle neotree)
  :bind
  ;; F8でneotree-windowが開くようにする
  ([f8] . neotree-toggle)
  )

;;;
;;; symbol-overlay
;;;
(el-get-bundle wolray/symbol-overlay  :depends (seq))
(require 'symbol-overlay)
(add-hook 'prog-mode-hook #'symbol-overlay-mode)
(add-hook 'markdown-mode-hook #'symbol-overlay-mode)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(define-key symbol-overlay-mode-map (kbd "C-c C-p") 'symbol-overlay-jump-prev)
(define-key symbol-overlay-mode-map (kbd "C-c C-n") 'symbol-overlay-jump-next)
(define-key symbol-overlay-mode-map (kbd "C-g") 'symbol-overlay-remove-all)

;;;
;;; magit
;;;
(use-package magit
  :init
  (el-get-bundle magit)
  )

;;;
;;; egg
;;;
(use-package egg
  :disabled t  ;; use magit.
  :init
  (el-get-bundle egg)
  )


;;(el-get-bundle redguardtoo/eacl)

;;;
;;; rainbow delimiters
;;;
;; referenced url:
;;  https://github.com/Fanael/rainbow-delimiters
;;  https://github.com/sellout/emacs-color-theme-solarized/issues/165
;;  https://qiita.com/megane42/items/ee71f1ff8652dbf94cf7
(use-package rainbow-delimiters
  :init
  (el-get-bundle rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  (outline-minor-mode t)
  (outline-minor-mode nil)
  (require 'cl-lib)
  (require 'color)
  (defun rainbow-delimiters-using-stronger-colors()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))  ;;; not working.
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)
  )

;;;
;;;  company-jedi
;;;
;; referenced url:
;;  http://tkf.github.io/emacs-jedi/latest/
;;  https://github.com/syohex/emacs-company-jedi
;;
(el-get-bundle company-jedi :depends (company-mode))
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(el-get-bundle helm-pydoc)
;;(el-get-bundle anaconda-mode)


;;;
;;;  golden ratio
;;;
;; referenced url: https://qiita.com/blue0513/items/ff8b5822701aeb2e9aae
;; golden ratio
(use-package golden-ratio
  :init
  ;;  (golden-ratio-mode 1) ;; Enable manually
  :config
  (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*Calendar*")
  )
;; active window move
(global-set-key (kbd "<C-left>")  'windmove-left)
(global-set-key (kbd "<C-down>")  'windmove-down)
(global-set-key (kbd "<C-up>")    'windmove-up)
(global-set-key (kbd "<C-right>") 'windmove-right)


;;;
;;;  misc setting
;;;


;; referenced url: https://qiita.com/kakikubo/items/412715e378b03b79faff
;; shut up, emacs!
(setq display-warning-minimum-level :error)

;; eshell
(setq eshell-prompt-function 'my-eshell-prompt)

(defun my-eshell-prompt ()
  "Eshell prompt setting."
  " $ "
  )

;; reload setting
(global-set-key [f12] 'eval-buffer)


;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))


;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

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

;; referenced url: http://d.hatena.ne.jp/crosshope/20110602/1306947203
;; 日付挿入
(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))


;; referenced url: https://qiita.com/icb54615/items/b04be54caf46d2bf721a
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key (kbd "C-c C-w") 'window-resizer) ;; optimaized for me

;;; init.el ends here
