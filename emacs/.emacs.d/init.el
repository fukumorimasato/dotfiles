;;; Init.el -- fukumori's emacs setting file ;; _*_ coding:utf-8-unix _*_

;; Filename: init.el
;; Package-Requires:
;; author: fukumori.masato.dev@gmail.com
;; version:
;; URL:

;;; commentary:

;;; code:

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
(defconst user-home-dir "~")
(defconst user-doc-dir  (concat user-home-dir "/Document"))
(when machine-linux
  (defconst user-doc-dir  (concat user-home-dir "/doc")))

(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;;;
;;;  proxy setting
;;;
;; load *.el files under proxy-dir
;;(defconst proxy-file "proxy/myproxy.el")
;;(load (locate-user-emacs-file proxy-file) t)


;;;
;;;  Setting for package.el
;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;;
;;;  Setting for use-package
;;;
;;;;; ensure to use use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;
;;;  package install by use-package
;;;
(use-package key-chord          :ensure t :config (key-chord-mode 1))
(use-package use-package-chords :ensure t)

;;;
;;; install el-get
;;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (package-install 'el-get)
  (require 'el-get)
  (el-get-elpa-build-local-recipes)
  )

;;;
;;;  color theme
;;;
(use-package monokai-theme :init (el-get-bundle monokai-theme))
(use-package atom-dark-theme :init (el-get-bundle atom-dark-theme))
(use-package zenburn-theme :init (el-get-bundle zenburn-theme))

(defun my-theme-setup-hook ()
  (interactive)
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
;;; magit
;;;
(use-package magit
  :init
  ;;(el-get-bundle magit :depends (dash magit-popup ghub graphql treepy with-editor))
  (el-get-bundle magit)
  :bind
  ("M-g" . magit-status)
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
    ;; ubuntuでインストールした場合
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
  (el-get-bundle emacs-helm/helm :branch "v2.8.8")
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

;; goto-line
(global-set-key "\C-x\C-g" 'goto-line)

(line-number-mode t)
;; set-goal-columは邪魔なので無効化
;; https://kb.iu.edu/d/abvd
(put 'set-goal-column 'disabled t)
(column-number-mode t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match "grey")
(set-face-foreground 'show-paren-match "black")

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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
 '(package-selected-packages '(zenburn-theme use-package-chords key-chord use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
