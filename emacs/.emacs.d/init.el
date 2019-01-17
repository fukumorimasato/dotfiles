;;; Init.el -- fukumori's emacs setting file ;; _*_ coding:utf-8-unix _*_

;; Filename: init.el
;; Package-Requires:
;; author: fukumori.masato.dev@gmail.com
;; version:
;; URL:

;;; commentary:

;;; code:

(package-initialize)

;;;
;;;  directory isolationp
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
(defconst user-home-dir "~")
(defconst user-doc-dir  (concat user-home-dir "/Document"))
(when machine-linux
  (defconst user-doc-dir  (concat user-home-dir "/doc")))
(defconst user-org-dir (concat user-doc-dir "/org"))

(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

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
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;;;
;;;  Setting for use-package
;;;
;;;;; ensure to use use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;
;;;  package install by use-package and el-get
;;;
(use-package key-chord          :ensure t :config (key-chord-mode 1))
(use-package use-package-chords :ensure t)

;;;
;;; install el-get
;;;
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (package-install 'el-get)
  (require 'el-get)
  (el-get-elpa-build-local-recipes))

;;;
;;;  color theme
;;;
(use-package solarized-theme
  :init
  (el-get-bundle color-theme-solarized)
  (set-terminal-parameter nil 'background-mode 'dark)
  (set-frame-parameter nil 'background-mode 'dark)
;;    (set-terminal-parameter nil 'background-mode 'light)
;;    (set-frame-parameter nil 'background-mode 'light)
  )
(use-package monokai-theme :init (el-get-bundle monokai-theme))
(use-package sublime-themes :init (el-get-bundle sublime-themes) :no-require t)
(use-package atom-dark-theme :init (el-get-bundle atom-dark-theme))
(use-package atom-one-dark-theme :init (el-get-bundle atom-one-dark-theme))
(use-package zenburn-theme :init (el-get-bundle zenburn-theme))

(defun my-theme-setup-hook ()
  (interactive)
  ;;  (load-theme 'solarized t)
  (load-theme 'monokai t)
  )
(my-theme-setup-hook)
(add-hook 'tty-setup-hook 'my-theme-setup-hook)

;;;
;;;  rainbow-mode
;;;
(use-package rainbow-mode
  :init
  (el-get-bundle emacsmirror:rainbow-mode)
  )

;;;
;;; powerline
;;;
(use-package powerline
  :init
  (el-get-bundle powerline)
  (el-get-bundle emacsmirror:xpm)

  ;; http://n8.hatenablog.com/entry/2012/03/21/172928
  ;; https://qiita.com/itome0403/items/c0fba8186ee8910bf8ab

  (defun powerline-my-theme ()
    "Setup the my mode-line."
    (interactive)
    (setq-default mode-line-format
		  '("%e"
		    (:eval
		     (let* ((active (powerline-selected-window-active))
			    (mode-line (if active 'mode-line 'mode-line-inactive))
			    (face0 (if active 'mode-line-color-active-0 'mode-line-color-inactive-0))
			    (face1 (if active 'mode-line-color-active-1 'mode-line-color-inactive-1))
			    (face2 (if active 'mode-line-color-active-2 'mode-line-color-inactive-2))
			    (face3 (if active 'mode-line-color-active-3 'mode-line-color-inactive-3))
			    (face4 (if active 'mode-line-color-active-4 'mode-line-color-inactive-4))
			    (powerline-current-separator 'utf-8)
			    (separator-left (intern (format "powerline-%s-%s"
							    (powerline-current-separator)
							    (car powerline-default-separator-dir))))
			    (separator-right (intern (format "powerline-%s-%s"
							     (powerline-current-separator)
							     (cdr powerline-default-separator-dir))))
			    (lhs (list (powerline-raw " %* %m  " face0)
				       (funcall separator-left face0 face1)
				       (powerline-raw "%Z  " face1)
				       (funcall separator-left face1 face2)
				       (powerline-raw " %b   " face2)
				       (funcall separator-left face2 face3)
				       ))
			    (rhs (list (funcall separator-right face3 face2)
				       (powerline-vc face2)
				       (funcall separator-right face2 face1)
				       (powerline-raw " %p " face1)
				       (funcall separator-right face1 face0)
				       (powerline-raw "    %4l:%3c " face0)
				       (powerline-fill face0 0)
				       )))
		       (concat (powerline-render lhs)
			       (powerline-fill face3 (powerline-width rhs))
			       (powerline-render rhs)))))))
    
  (defun make/set-face (face-name bg-color fg-color  weight inherit)
    (make-face face-name)
    (set-face-attribute face-name nil
			:foreground fg-color :background bg-color
			:box nil :weight weight :inherit inherit))
  
  (make/set-face 'mode-line-color-active-0 "#ffffd7" "#5f5faf" 'bold 'mode-line)
  (make/set-face 'mode-line-color-active-1 "#ffffd7" "#00afaf" 'bold 'mode-line)
  (make/set-face 'mode-line-color-active-2 "#ffffd7" "#d33682" 'bold 'mode-line)
  (make/set-face 'mode-line-color-active-3 "#ffffd7" "#585858" 'bold 'mode-line-inactive)
  (make/set-face 'mode-line-color-active-4 "#ffffd7" "#93a1a1" 'bold 'mode-line)
  (make/set-face 'mode-line-color-inactive-0 "#ffffd7" "#585858" 'bold 'mode-line-inactive)
  (make/set-face 'mode-line-color-inactive-1 "#ffffd7" "#808080" 'bold 'mode-line-inactive)
  (make/set-face 'mode-line-color-inactive-2 "#ffffd7" "#585858" 'bold 'mode-line-inactive)
  (make/set-face 'mode-line-color-inactive-3 "#ffffd7" "#93a1a1" 'bold 'mode-line-inactive)
  (make/set-face 'mode-line-color-inactive-4 "#ffffd7" "#585858" 'bold 'mode-line-inactive)
  
  (powerline-my-theme)
  )

;;
;; ag
;;
(use-package ag
  :diminish ""
  :init
  (el-get-bundle ag)
  )

(use-package helm-ag
  :diminish ""
  :init
  (el-get-bundle helm-ag)
  )

;;;
;;; migemo
;;;
;; referenced url: https://github.com/tsu-nera/dotfiles/blob/master/.emacs.d/inits/20_text.org
(use-package migemo
  :diminish ""
  :init
  (el-get-bundle migemo)
  :config
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  (when machine-linux
    ;; ubuntu 16.04 LTS apt-get でインストールした場合
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))

  (when machine-mac
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/local/Cellar/cmigemo/HEAD-5c014a8/share/migemo/utf-8/migemo-dict/usr/local/Cellar/cmigemo/HEAD-5c014a8/sha"))
  
;;  (when windows-p
;;    (setq migemo-command "c:/app/cmigemo-default-win64/cmigemo.exe")
;;    (setq migemo-dictionary "c:/app/cmigemo-default-win64/dict/utf-8/migemo-dict"))
  (migemo-init)
  )

;;;
;;; helm
;;;
(use-package helm
  :diminish ""
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
  (helm-migemo-mode 1)
  )

;;;
;;; helm-themes
;;;
(use-package helm-themes
  :init
  (el-get-bundle helm-themes)
  )

;;;
;;; helm-projectile/projectile
;;;
(use-package helm-projectile
  :diminish ""
  :init
  (el-get-bundle helm-projectile)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode)
  (helm-projectile-on)
  )

;;;
;;; helm-swoop
;;;
(use-package helm-swoop
  :init
  (el-get-bundle helm-swoop)
  :bind
  ("C-s" . helm-swoop)
  )

;;;
;;;  helm-descbinds
;;;
(use-package helm-descbinds
  :diminish ""
  :init
  (el-get-bundle helm-descbinds)
  :config
  (helm-descbinds-mode 1)
  )

;;;
;;;  dumb-jump
;;;
(use-package dumb-jump
  :diminish ""
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
  :diminish ""
  :init
  (el-get-bundle joaotavora/yasnippet)
  (el-get-bundle helm-c-yasnippet)
  (el-get-bundle yasnippet-snippets)

  :bind
  (:map yas-minor-mode-map
   ("C-x & C-n" . nil)
   ("C-x & C-s" . nil)
   ("C-x & C-v" . nil)
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
  :diminish ""
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

;;;
;;;  company-jedi
;;;
(use-package company-jedi
  :init
  (el-get-bundle jedi-core)
  (el-get-bundle company-jedi :depends (company-mode))
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

;;;
;;;  flycheck: syntacks check.
;;;
(use-package flycheck
  :diminish ""
  :init
  (el-get-bundle flycheck)
  (el-get-bundle yasuyk/helm-flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  :bind
  ("C-c C-n" . flycheck-next-errors)
  ("C-c C-p" . flycheck-previous-errors)
  ("C-c C-l" . flycheck-list-errors)
  ("C-c C-c" . helm-mode-flycheck-compile)
  ("C-c C-f" . helm-flycheck)
  )

;;;
;;;  elpy
;;;  require: pip install ipython jedi flake8 autopep8 yapf
;;;
(use-package elpy
  :init
  (el-get-bundle elpy)
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-rpc-backend "jedi")
  )

;;; https://github.com/naiquevin/sphinx-doc.el/tree/f39da2e6cae55d5d7c7ce887e69755b7529bcd67
(use-package sphinx-doc
  :diminish ""
  :init
  (el-get-bundle sphinx-doc)
  :config
  (add-hook 'python-mode-hook (lambda () (sphinx-doc-mode t)))
  )

;;;
;;;  helm-pydoc
;;;
(use-package helm-pydoc
  :init
  (el-get-bundle helm-pydoc)
  )

;;;
;;; emacs-async
;;;
(use-package async
  :init
  (el-get-bundle emacs-async)
  )

;;;
;;; hlinum: 左側に行を表示する.
;;;
(el-get-bundle tom-tan/hlinum-mode)
(global-linum-mode t)
(set-face-foreground 'linum "green")
(setq linum-format "%5d ")
(require 'hl-line)
;;(set-face-foreground 'hl-line "#3FC")
;;(set-face-background 'hl-line "green")
;;(set-face-attribute 'hl-line nil :inherit nil)
(global-hl-line-mode)
(require 'hlinum)
(hlinum-activate)
(set-face-foreground 'linum-highlight-face "#3FC")
(set-face-background 'linum-highlight-face "green")

;;;
;;; smooth-scroll
;;;
(use-package smooth-scroll
  :diminish ""
  :init
  (el-get-bundle k-talo/smooth-scroll
    :type git
    :url "https://github.com/k-talo/smooth-scroll.el")
  
  :config
  (smooth-scroll-mode)
  )


;;;
;;;  neotree: Display directory tree.
;;;
(use-package neotree
  :diminish ""
  :init
  (el-get-bundle neotree)
  :bind
  ;; F8でneotree-windowが開くようにする
  ([f8] . neotree-toggle)
  )

;;;
;;; symbol-overlay
;;;
(use-package symbol-overlay
  :diminish ""
  :init
  (el-get-bundle wolray/symbol-overlay  :depends (seq))
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode)
  :bind
  (("M-i" . symbol-overlay-put)
   (:map symbol-overlay-mode-map
	 ("C-c C-p" . symbol-overlay-jump-prev)
	 ("C-c C-n" . symbol-overlay-jump-next)
	 ("C-g" . symbol-overlay-remove-all)
	 )
   )
  )
  
;;;
;;; magit
;;;
(use-package magit
  :init
  (el-get-bundle magit)
  :bind
  ("M-g" . magit-status)
  )

;;;
;;; rainbow delimiters
;;;
;; referenced url:
;;  https://github.com/Fanael/rainbow-delimiters
;;  https://github.com/sellout/emacs-color-theme-solarized/issues/165
;;  https://qiita.com/megane42/items/ee71f1ff8652dbf94cf7
(use-package rainbow-delimiters
  :diminish ""
  :init
  (el-get-bundle rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  (outline-minor-mode t)
  (outline-minor-mode nil)
  )

;;;
;;;  golden ratio
;;;
;; referenced url: https://qiita.com/blue0513/items/ff8b5822701aeb2e9aae
;; golden ratio
(use-package golden-ratio
  :init
  (el-get-bundle golden-ratio)
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
;;; org-mode
;;;
(use-package org
  :ensure t
  :init
  (setq org-agenda-files (list user-org-dir))
  (setq my-org-todo-file (concat user-org-dir "/todo.org"))
  (setq my-org-tech-file (concat user-org-dir "/tech.org"))
  (setq my-org-patent-file (concat user-org-dir "/patent.org"))
  (setq my-org-meeting-file (concat user-org-dir "/meeting.org"))
  (setq my-org-home-file (concat user-org-dir "/home.org"))
  (setq my-org-home-file (concat user-org-dir "/kendo.org"))  
  (setq org-capture-templates
	'(("t" "Todo" entry (file (expand-file-name my-org-todo-file))
	   "* TODO %?\n    %T")
	  ("g" "Tech" entry (file (expand-file-name my-org-tech-file))
	   "* %?\n    %T")
	  ("p" "Patent" entry (file (expand-file-name my-org-patent-file))
	   "* %?\n    %T")
	  ("m" "Meeting" entry (file+datetree (expand-file-name my-org-meeting-file))
	   "* %?\n    %T" :jump-to-captured 1)
	  ("h" "Home" entry (file (expand-file-name my-org-home-file))
	   "* %?\n    %T")	   	   
	  ("k" "kendo" entry (file (expand-file-name my-org-home-file))
	   "* %?\n    %T")	   	   
	  ))
  :bind
  ("C-c q" . org-capture)
  ("C-c a" . org-agenda)
  )

;;;
;;;  for markdown
;;;  https://github.com/jrblevin/markdown-mode
;;;  https://www.yokoweb.net/2017/01/08/emacs-markdown-mode/
;;;  https://github.com/ancane/markdown-preview-mode
;;;
(use-package markdown-mode
  :init
  (el-get-bundle markdown-mode)
  (setq markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  )

(use-package markdown-preview-mode
  :init
  (el-get-bundle markdown-preview-mode)
  :config
  (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
  )

(use-package markdown-toc
  :init
  (el-get-bundle markdown-toc)
  )


;;;
;;;  for reStructuredText
;;;
(use-package rst-mode
  :init
  (el-get-bundle rst-mode)
  (setq frame-background-mode 'dark)
  :mode (("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Ctrl+hをbackspaceとして使用する
;; http://akisute3.hatenablog.com/entry/20120318/1332059326
;;(keyboard-translate ?\C-h ?\C-?)
;; http://malkalech.com/emacs_c-h_backspace
(define-key key-translation-map [?\C-h] [?\C-?])

;; referenced url: https://qiita.com/kakikubo/items/412715e378b03b79faff
;; shut up, emacs!
(setq display-warning-minimum-level :error)

;; eshell
(setq eshell-prompt-function 'my-eshell-prompt)

(defun my-eshell-prompt ()
  "Eshell prompt setting."
  "[eshell]$ "
  )


;; locale
(set-locale-environment nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)

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

;; set-goal-columは邪魔なので無効化
;; https://kb.iu.edu/d/abvd
(put 'set-goal-column 'disabled t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match "grey")
(set-face-foreground 'show-paren-match "black")

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
(global-set-key (kbd "C-c d") 'insert-current-time)


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
(global-set-key (kbd "C-c w") 'window-resizer)

(load (locate-user-emacs-file "my-coding-style.el") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(conda-anaconda-home "~/.pyenv/versions/anaconda3-5.1.0")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (zenburn-theme atom-one-dark-theme sublime-themes markdown-preview-mode helm el-get use-package-chords key-chord use-package)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
