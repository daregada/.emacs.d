;;; init.el --- Emacs early init file for EDU system (logos). -*- lexical-binding: t; -*-
;;; Commentary:
;; Edu(logos)用のearly-init.el

;;; Code

;; (setq debug-on-error t)

;; いらないバーを非表示にする
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; GUI用設定。まだwindow-systemによる判定は使えない
(setq initial-frame-alist
      (append (list
               '(width . 100)
               '(height . 40)
               '(top . 0)
               '(left . 960)
               '(font . "UDEV Gothic NF-13.5")
               '(alpha . 90)
               )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)
