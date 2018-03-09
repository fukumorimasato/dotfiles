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

;;;
;;;  color theme
;;;
;;  adapt solarized
(use-package solarized-theme
  :init
  (el-get-bundle color-theme-solarized)
  (defun my-theme-setup-hook ()
    (interactive)
;;    (set-terminal-parameter nil 'background-mode 'dark)
;;    (set-frame-parameter nil 'background-mode 'dark)
    (set-terminal-parameter nil 'background-mode 'light)
    (set-frame-parameter nil 'background-mode 'light)
    (load-theme 'solarized t)
    )
  (my-theme-setup-hook)
  (add-hook 'tty-setup-hook 'my-theme-setup-hook)
  )

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
				       ))
			    (rhs (list (powerline-vc face2)
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
  )

;;
;; helm-projectile/projectile
;;
(use-package helm-projectile
  :diminish ""
  :init
  (el-get-bundle helm-projectile)
  :config
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
;;;  conda
;;;
;; https://github.com/necaris/conda.el
(use-package conda
  :diminish ""
  :init
  (el-get-bundle necaris/conda
    :type git
    :url "https://github.com/necaris/conda.el"
    :depends (pythonic virtualenvwrapper))
  (custom-set-variables
   '(conda-anaconda-home "~/.pyenv/versions/anaconda3-5.1.0"))
  (conda-env-autoactivate-mode t)
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
;; referenced url:
;;  http://tkf.github.io/emacs-jedi/latest/
;;  https://github.com/syohex/emacs-company-jedi
;;  https://github.com/syohex/emacs-company-jedi/wiki
;;
(use-package company-jedi
  :if (fboundp 'company-mode)
  :diminish ""
  :init
  (el-get-bundle company-jedi)
;;  (setq jedi:environment-virtualenv (list (expand-file-name (locate-user-emacs-file ".python-environments/"))))
  (setq jedi:server-args
	'("--sys-path" "~/.pyenv/versions/anaconda3-5.1.0/lib/python3.6"
	  "--sys-path" "~/.pyenv/versions/anaconda3-5.1.0/lib/python3.6/lib-dynload"
	  "--sys-path" "~/.pyenv/versions/anaconda3-5.1.0/lib/python3.6/site-packages"
	  ;; ... and more! ...
	  ))
  (defun my/company-jedi-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/company-jedi-mode-hook)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  )


(use-package company-anaconda
  :disabled t
  :init
  (el-get-bundle company-anaconda :depends (anaconda-mode))
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)  
  (defun my/company-anaconda-mode-hook ()
    (add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook 'my/company-anaconda-mode-hook)
;;  (add-to-list 'company-backends '(company-anaconda :with company-capf))
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
;;;  flycheck: syntacks check.
;;;
(use-package flycheck
  :diminish ""
  :init
  (el-get-bundle flycheck)
  (el-get-bundle yasuyk/helm-flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)  
  :bind
  ("C-c C-n" . flycheck-next-errors)
  ("C-c C-p" . flycheck-previous-errors)
  ("C-c C-l" . flycheck-list-errors)
  ("C-c C-c" . helm-mode-flycheck-compile)
  ("C-c C-f" . helm-flycheck)
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
  ("C-c C-g" . magit-status)
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
  :diminish ""
  :init
  (el-get-bundle rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  (outline-minor-mode t)
  (outline-minor-mode nil)
  )

(use-package py-autopep8
  :diminish ""
  :init
  (el-get-bundle py-autopep8)
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
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

;; referenced url: https://qiita.com/kakikubo/items/412715e378b03b79faff
;; shut up, emacs!
(setq display-warning-minimum-level :error)

;; eshell
(setq eshell-prompt-function 'my-eshell-prompt)

(defun my-eshell-prompt ()
  "Eshell prompt setting."
  " $ "
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
(put 'set-goal-column 'disabled nil)


;; 対応する括弧を光らせる
(show-paren-mode 1)


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
