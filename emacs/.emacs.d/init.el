;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  共通設定
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(set-frame-parameter nil 'background-mode 'light) 
(set-terminal-parameter nil 'background-mode 'light) 
(load-theme 'solarized t)

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

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
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)


;; 対応する括弧を光らせる
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")

;; スペース、タブなどを可視化する
;;(global-whitespace-mode 1)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
;;(global-linum-mode t)

(defun find-tag-next ()
  (interactive)
  (find-tag last-tag t))

(global-set-key (kbd "M-g .")   'find-tag-regexp)
(global-set-key (kbd "C-M-.")   'find-tag-next)
(global-set-key (kbd "M-,")     'find-tag-other-window)
(global-set-key (kbd "M-g M-.") 'anything-c-etags-select)

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
 ;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;;
;; indent(default) => TAB (8文字幅)
;;
(setq-default indent-tabs-mode t tab-width 8)

;;
;; don't ask whether open symbolik linked file or not
;;
(setq vc-follow-symlinks t)

