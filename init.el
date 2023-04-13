;;; init.el --- Emacs init file for EDU system (logos). -*- lexical-binding: t; -*-

;;; Commentary:
;; Edu(logos)用のinit.el

;;; Code:

(require 'cl-lib)
(eval-and-compile
  (defvar in-development t))

;; -lオプションで指定した/バイトコンパイルする設定ファイルにuser-emacs-directoryを合致させる
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))


;; 軽量バッファーモード
(defun lightweight-buffer-mode ()
  "Lightweight Buffer Mode for novice."
  (interactive)
  (setq mode-name "Lightweight Buffer")
  (setq major-mode 'lightweight-buffer-mode)
  (run-hooks 'lightweight-buffer-mode-hook))


;; パッケージインストール時に*scratch*にパッケージ名を表示
(defun print-package-name-in-scrach-buffer (pkg)
    (with-current-buffer (get-buffer "*scratch*")
      (insert (format "%sパッケージを導入中\n" pkg))
      (redisplay))
  )

;;
;; 起動処理後の表示をフック
;;
(if (version< emacs-version "27.0")
  (with-current-buffer (get-buffer "*scratch*")
    (insert "Emacsのバージョンが古くて動きません。\n"
            "新しいEmacsを優先起動する設定の.bash_profileを作っていないのでは。\n\n")
    )

  (advice-add 'package-install :before #'print-package-name-in-scrach-buffer)

  ;;(advice-add 'command-line-1 :after #'display-notes)
  (add-hook 'emacs-startup-hook #'display-notes)
  )

(defun display-notes (&rest _ignored)
  ;; *scratch*に起動後の注意書きを表示
  "Display notes for novice user."
  
  (with-current-buffer (get-buffer "*scratch*")
    (when (not (one-window-p))
      (delete-windows-on (get-buffer "*Compile-Log*")))
    
  ;; 先頭行をヘッダーラインに
  ;; (dolist (color-name (defined-colors))
  ;;   (message color-name))
  ;;(y-or-n-p "Continue?")
    (setq header-line-format
          (concat " 注意: ここにC言語のプログラムを書かないでください。"))

    ;; 注意書きを挿入
    (goto-char (point-max))
    (insert "\nC言語のソースファイル(拡張子.c)を作成/開いて、そこにプログラムを書いてください。\n\n"
            "ファイルを作成/開くには、キー操作(C-x C-f)を使います。\n"
            "(「C-x」はCtrlキーを押したままXキーを押す操作。その後の「C-f」も同様)\n"
            "一番下に「Find File:」と出たら、ファイル名を入力しEnterキーを押してください。\n\n"
            
            "ヒント: 演習用のファイル(progNN.c)であれば、もっと簡単な方法があります。\n"
            "「<f7>」(F7キー)を押し、一番下に「Program Number:」と出たら、\n"
            "プログラム番号(1桁か2桁の整数)を入力し、Enterキーを押してください。\n\n"
            "起動時の処理が完了しました。\n")
    
    (set-buffer-modified-p nil)
    (face-remap-add-relative 'header-line
                             :foreground (if (available-truecolor-p) "#1c1c1c" "color-234")
                             :background (if (available-truecolor-p) "#ffaf00" "color-214"))

    (advice-remove 'package-install #'print-package-name-in-scrach-buffer)
))





;; leafの読み込み
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    
    (with-current-buffer (get-buffer "*scratch*")
      (lightweight-buffer-mode)
      (setq header-line-format
            (concat " 起動時の処理を実行中です。"))

      (insert "\n"
              "パッケージの自動導入作業を行ないます。\n"
              "導入作業が終わるまでしばらく時間がかかります。\n"
              "最上行がオレンジ色になるまでお待ちください。\n\n")
      (redisplay))
    
    (package-refresh-contents)
    (package-install 'leaf))


  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    ;; (leaf hydra :ensure t)
    ;; (leaf el-get :ensure t)
    ;; (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)
    )

  ;; leaf-tree関連 開発時のみ
  (leaf leaf-tree
    :ensure t imenu-list
    :disabled (not in-development)
    :custom
    (
     (imenu-list-size . 30)
     (imenu-list-position . 'left)
     (leaf-tree-flat . nil)
     )
    :bind
    (("M-t" . leaf-tree-mode))
    )


  ;; leaf-convert関連 開発時のみ
  (leaf leaf-convert
    :ensure t
    :disabled (not in-development)
    )

  )

;; GUI用の設定。一部はearly-init.elに移動(起動時のちらつきを抑えるため)
(leaf *window-system-settings
  :doc "settings for window-system"
  :when (window-system)
  :config
  (modify-frame-parameters nil '((sticky . t) (width . 100) (height . 40)))

  (set-face-attribute 'default nil
                      :family "UDEV Gothic NF"
                      :height 136)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (cons "UDEV Gothic NF" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0212
                    (cons "UDEV Gothic NF" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    (cons "UDEV Gothic NF" "iso10646-1"))

  )


(provide 'init)



;; カスタムファイルの指定。カスタム設定が custom.el に分離されるようになる
;; 保存されたカスタムファイルを(わざと)読み込まない
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom
  `((custom-file . ,(locate-user-emacs-file "custom.el"))))


;; 静かに起動
(leaf startup
  :custom
  (
   ;; Emacsのスタート画面を消す
   (inhibit-startup-screen            . t)
   ;; 起動時にscratchバッファー先頭のメッセージを出さない
   (initial-scratch-message           . nil)
   )
  :config
  ;; 起動時にミニバッファにメッセージを表示
  (defun display-startup-echo-area-message ()
    (message "Emacsを起動しました。"))
  ;; Emacs 28.1以降のNative Compilationの警告を抑制
  (unless (version< emacs-version "28.1")
    (custom-set-variables '(native-comp-async-report-warnings-errors 'silent)))

  )


;; EmacsのC言語部分で定義されている変数など
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom-face
  ;; 選択部分の色
  ((region . '((((background dark)) (:background "#0078d7" :foreground "#101010"))
               (t                   (:background "#0078d7" :foreground "#101010"))))
   )
  :custom
  (
   ;; (user-full-name . "YOUR NAME")
   ;; (user-mail-address . "hogehoge@example.com")
   ;; (user-login-name . "hogehoge")
   (create-lockfiles . nil)
   ;; (debug-on-error . t)
   (init-file-debug . t)
   (frame-resize-pixelwise . t)
   (enable-recursive-minibuffers . t)
   (history-length . 1000)
   (history-delete-duplicates . t)
   (scroll-preserve-screen-position . t)
   (scroll-conservatively . 100)
   (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
   (ring-bell-function . 'ignore)
   (text-quoting-style . 'straight)
   (truncate-lines . t)
   ;; (use-dialog-box . nil)
   ;; (use-file-dialog . nil)
   ;; (menu-bar-mode . nil)
   ;; (tool-bar-mode . nil)
   ;; (scroll-bar-mode . nil)
   ;; 字下げにタブ文字を使わない
   (indent-tabs-mode . nil)
   )
  :config
  ;; (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Ctrl-h を DEL に変換
  (keyboard-translate ?\C-h ?\C-?)
  ;; 全角スペース"　"などnobreak-spaceに追加された文字の特殊表示を無効
  (unless (version< emacs-version "28.0")
    (setq nobreak-char-display nil))
  ;; rawバイトは16進数で表示(Emacs 26.1以降で有効)
  (unless (version< emacs-version "26.1")
    (setq display-raw-bytes-as-hex t))
  )


;; 外部でファイルが書き換えられたときの対応
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom
  (
   (auto-revert-interval . 1)
   )
  :global-minor-mode global-auto-revert-mode)


;; 選択中にキー入力すると選択部分が削除される
(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)


;; カッコの強調表示
(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :defvar (show-paren-when-point-inside-paren show-paren-when-point-in-periphery show-paren-style)

  :mode-hook
  (c-mode-common-hook . ((setq-local show-paren-style 'expression)
                         (setq-local show-paren-when-point-inside-paren nil)
                         (setq-local show-paren-when-point-in-periphery nil)
                         (face-remap-add-relative 'show-paren-match 'my-paren-match-remap-style)
                         (setq-local show-paren-mode t)
                         ))
  (emacs-lisp-mode-hook . ((setq-local show-paren-style 'expression)
                           (setq-local show-paren-when-point-inside-paren nil)
                           (setq-local show-paren-when-point-in-periphery nil)
                           (face-remap-add-relative 'show-paren-match 'my-paren-match-remap-style)
                           (setq-local show-paren-mode t)
                           ))
  (lightweight-buffer-mode-hook . ((setq-local show-paren-mode nil)))
  
  :preface
  ;; TrueColor環境かどうかのチェック
  (defun available-truecolor-p ()
    "Return t if truecolor is available."
    (interactive)
    (let ((result (or (display-graphic-p)
                      (eq (tty-display-color-cells) 16777216))))
      (when (called-interactively-p 'any)
        (message (format "%s" result)))
      result))
  
  :custom
  (
   (show-paren-delay . 0.1)
   (show-paren-style . 'parenthesis)
   (show-paren-mode . nil)
   (show-paren-priority . -1)
   )
  
  :config
  (defface my-paren-match-remap-style
    `((((background dark)) (:background ,(if (available-truecolor-p) "#4e4e4e" "color-239")))
      (t                   (:background ,(if (available-truecolor-p) "#e4e4e4 ""color-254"))))
      "Customized paren match face"
      :group 'init
      )
  )


;; simple関連
(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom
  ((kill-ring-max . 100)
   (kill-read-only-ok . t)
   (kill-whole-line . t)
   (eval-expression-print-length . nil)
   (eval-expression-print-level . nil)
   ;; 閉じカッコ入力時のハイライト表示
   (blink-matching-paren . nil)
           
   )
  )


;; whitespace関連
(leaf whitespace
  :doc "visualize tab, space, nobreak-space"
  :tag "builtin"
  :hook
  ((c-mode-common-hook   . whitespace-mode)
   (emacs-lisp-mode-hook . whitespace-mode))
  :custom-face
  ((whitespace-hspace . '((t (:background "pink" :foreground "black"))))
   (whitespace-tab . '((t (:background "lightgray" :foreground "red" :underline t)))))
  :custom
  (
   ;; nobreak-spaceとtabの外見をwhitespaceで変更する
   (whitespace-style . '(face                            ; faceで可視化
                         ;; trailing                        ; 行末の空白
                         tabs                            ; タブ
                         spaces                          ; スペース(spaceとnobreak-spaceを両方含む)
                         ;; empty                           ; ファイル先頭/末尾の空行
                         ;; space-mark                      ; 空白表示のマッピング
                         ;; tab-mark                        ; タブ表示のマッピング
                         ))

   ;; 上記 spaces の設定では space と nobreak-space の両方が対象になる
   ;; spaceの正規表現。空文字列にできないので、実質的に空になる内容を設定
   (whitespace-space-regexp . "\\(\\)")
   ;; nobreak-spaceの正規表現
   (whitespace-hspace-regexp . "\\([\u00a0\u1680\u2001-\u200a\u202f\u205f\u3000]+\\)")
   ;; 対象とするグローバルモード
   (whitespace-global-modes . '(not eww-mode
                                    term-mode
                                    eshell-mode
                                    org-agenda-mode
                                    calendar-mode))
   ;; 表示サンプル \u3000[　]  \u00a0[ ] tab[		]  
   )
  )


;; 端末でもマウスを使用可能に
(leaf xt-mouse
  :doc "Enable mouse in terminal"
  :tag "builtin"
  :unless (window-system)
  :global-minor-mode xterm-mouse-mode
  )


;; マウス関連(パッケージなし)
(leaf *mouse-support
  ;; 端末で以下の設定を有効にするには、xterm-mouse-modeにしておく必要がある
  :after xt-mouse
  :defvar (my-prev-buffer-name)
  :commands switch-window-buffer-to-flycheck-errors-from
  ;; マウス操作で*shell*や*compilation*バッファからソースコードに切り替えた時に*Flycheck errors*を表示
  :advice
  ;; (advice-add 'mouse-set-point :before #'mouse-set-point-before)
  (:before mouse-set-point my-mouse-set-point-before)
  ;; (advice-add 'mouse-set-point :after #'mouse-set-point-after)
  (:after  mouse-set-point my-mouse-set-point-after)
  :bind
  (
   ;; 右・中クリックでリージョンをコピー
   ("<mouse-2>" . copy-region-as-kill)
   ("<mouse-3>" . copy-region-as-kill)

   ;; ホイールで1行ずつスクロール
   ("<mouse-4>" . (lambda () (interactive) (scroll-down 1)))
   ("<mouse-5>" . (lambda () (interactive) (scroll-up   1)))


   ;; Alt-ホイールで5行ずつスクロール
   ("M-<mouse-4>" . (lambda () (interactive) (scroll-down 10)))
   ("M-<mouse-5>" . (lambda () (interactive) (scroll-up   10)))
   )
  :preface
  (defun my-mouse-set-point-before (&rest _ignored)
    (setq my-prev-buffer-name (buffer-name)))

  (defun my-mouse-set-point-after (&rest _ignored)
    (unless (string= my-prev-buffer-name (buffer-name))
      ;; (message (concat "mouse-set-pointアドバイス実行中: " my-prev-buffer-name "->" (buffer-name)))
      (when (and (or (string= my-prev-buffer-name "*shell*")
                     (string= my-prev-buffer-name "*compilation*"))
                 (and (not (eq buffer-file-name nil))
                      (string= (file-name-extension buffer-file-name t) ".c")))
        (switch-window-buffer-to-flycheck-errors-from my-prev-buffer-name))))
  
  )


;; 行番号関連
(leaf display-line-numbers
  :doc "interface for display-line-numbers"
  :tag "builtin"
  :emacs>= 26.1
  :hook
  ((c-mode-common-hook . display-line-numbers-mode))
  
  :setq-default
  (
   ;; 行番号の桁数を固定
   (display-line-numbers-width . 5)
   )
  :custom-face
  ;; :background を設定すると、デフォルトの背景の透明度が引き継がれない
  ((line-number . '((((background dark)) (:foreground "white"))
                    (t                   (:foreground "black"))))
   (line-number-current-line . '((t (:background "gray" :foreground "black")))))

  )


;; キーカスタマイズ関連(パッケージなし)
(leaf *bind-global-keys
  :doc "binding some keys globally"
  :bind (
         ;; Ctrl-Vに「貼り付け」(yank)を割り当て(Windwosユーザー向け対策)
         ;; Ctrl-Vの元の機能「1画面下を表示」(scroll-up-command)はPagedownで代用可能
         ("C-v" . yank)
         
         ;; Ctrl-Zに「元に戻す」(undo)を割り当て(Windowsユーザー向け対策)
         ;; Ctrl-Zの元の機能「一時停止」(suspend-emacs)は「M-x suspend-emacs」で代用可能
         ("C-z" . undo)
         
         ;; F5キーとCtrl-C cとCtrl-C 5に、「編集中のプログラムを実行形式に変換する機能」を割り当て
         ;; エラーが発生した場合は、下のウィンドウがflycheck-errorsの表示に戻り
         ("<f5>" . 'save-and-compile)
         ("C-c c" . 'save-and-compile)
         ("C-c 5" . 'save-and-compile)
         
         ;; F6キーとCtrl-C sとCtrl-C 6に、「別ウィンドウ(または別バッファー)の選択」を割り当て
         ;; 画面が分割表示されているときは、ポインター(カーソル)が別ウィンドウに移る
         ;; (shellバッファーのウィンドウから移った場合は下のウィンドウがflycheck-errorsの表示に戻る)
         ;; 画面が分割表示されていないときは、表示内容が別のバッファー(ファイルや実行画面)に切り替わる
         ("<f6>" . other-window-or-buffer)
         ("C-c s" . other-window-or-buffer)
         ("C-c 6" . other-window-or-buffer)
         
         ;; F7キーとCtrl-C fとCtrl-C 7に、「指定した番号の演習用ファイル(progXX.c)を開く」を割り当て
         ("<f7>" . find-practice-file)
         ("C-c f" . find-practice-file)
         ("C-c 7" . find-practice-file)
         
         ;; F8キーとCtrl-C nとCtrl-C 8に、「行番号/ヘッダー表示の切り替え」を割り当て
         ("<f8>" . toggle-line-numbers-and-header)
         ("C-c n" . toggle-line-numbers-and-header)
         ("C-c 8" . toggle-line-numbers-and-header)

         ;; F9キーとCtrl-C rとCtrl-C 7に、「Emacsの再起動」を割り当て
         ("<f9>" . desktop-save-and-restart-emacs)
         ("C-c r" . desktop-save-and-restart-emacs)
         ("C-c 9" . desktop-save-and-restart-emacs)

         ;; F10～F12キーに、「WezTermの外見変更ガイダンス」を割り当て
         ;; Ctrl/Shift/Altキーと組み合わせた場合は、WezTerm側で処理される
         ("<f10>" . echo-guidance-for-customizing-wezterm-appearance)
         ("<f11>" . echo-guidance-for-customizing-wezterm-appearance)
         ("<f12>" . echo-guidance-for-customizing-wezterm-appearance)

         ;; Insertキーの無効化。間違って押して戻せない人が多いため
         ("<insertchar>" . (lambda ()
                             (interactive)
                             (message "Insertキーは無効です。上書きモードは M-x overwrite-mode で。")))

         )
  :preface
  ;; WezTerm外見変更用のガイダンスを表示
  (defun echo-guidance-for-customizing-wezterm-appearance ()
    "echo guidance for wezterm appearance cutomization"
    (interactive)
    (when (string= (getenv "TERM_PROGRAM") "WezTerm")
      (message "WezTermの外見変更には、CtrlキーかShiftキーかAltキーのどれかとF10～F12キーを組み合わせてください。")
      )
    )

  ;; 別ウィンドウ(または別バッファー)の選択
  (defun other-window-or-buffer ()
    (interactive)
    (if (one-window-p)
        ;; ウィンドウが分割されていなければ、バッファを切り換える
        (switch-to-buffer (other-buffer))
      ;; 非ファイルバッファーから別のウィンドウに移動するときはflycheck-errorsに切り替える
      (when (eq buffer-file-name nil)
        (switch-window-buffer-to-flycheck-errors-from (buffer-name)))
      ;; ウィンドウが分割されていれば、別ウィンドウを選択する
      (other-window 1))
    )

  ;; 指定した番号(通常は2桁)を持つ演習用プログラム「progXX.c」を開く
  (defun find-practice-file (number)
    "指定した番号(通常は2桁)を持つ演習用プログラム「progXX.c」を開く"
    (interactive "nProgram Number: ")
    (let ((file-name (format "~/prog%02d.c" number))
          )
      (when (eq buffer-file-name nil)
        (other-window 1))
      (find-file file-name)
      ))

  ;; 行番号とヘッダー行の表示をトグルする
  (defun toggle-line-numbers-and-header ()
    (interactive)
    ;; 行番号はいつでもトグル
    (when (fboundp 'display-line-numbers-mode)
      (call-interactively 'display-line-numbers-mode))
    
    ;; my-header-line-formatが存在する場合のみヘッダー行をトグル
    (when (and (boundp 'my-header-line-format)
               (eq major-mode 'c-mode))
      (if (eq header-line-format nil)
          (when (eq display-line-numbers nil)
            (setq header-line-format my-header-line-format))
        (when (eq display-line-numbers nil)
          (setq header-line-format nil))))
    )

  )


;; shell関連
(leaf shell
  :doc "specialized comint.el for running the shell"
  :custom
  (
   ;; プロンプトは読み取り専用
   (comint-prompt-read-only . t)
   )
  :config
  ;; プロンプトにコマンドの戻り値($?)を含める
  (setenv "PS1" "[\\u@\\h \\W ($(echo $?;))]\\$ ")

  )


;; which-function-mode関連(ファイル名はwhich-func.el)
(leaf which-func
  :doc "print current function in mode line"
  :tag "builtin"
  :custom
  (
   ;; 関数不明時の表示
   (which-func-unknown . "外部")
   ;; which-func-modes に設定したモードのみ有効
   (which-func-modes . '(c-mode emacs-lisp-mode))
   )
  :config
  ;; グローバルに有効化(which-func-modes 参照)
  (which-function-mode t)
  :defer-config
  (set-face-foreground 'which-func (if (available-truecolor-p) "#ffaf00" "color-214"))
  
  )


;; powerline関連
(leaf powerline
  :doc "Rewrite of Powerline"
  :ensure t
  :require cl-lib
  :commands powerline-my-theme
  :custom
  (
   (powerline-display-buffer-size . nil)
   (powerline-display-mule-info . nil)
   )

  :preface
  (defun powerline-my-theme ()
    "Setup my mode-line."
    (interactive)
    (setq-default
     mode-line-format
     '("%e"
       (:eval
        (let* ((active (powerline-selected-window-active))
               (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
               (mode-line (if active 'mode-line 'mode-line-inactive))
               (face0 (if active 'powerline-active0 'powerline-inactive0))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (separator-left (intern (format "powerline-%s-%s"
                                               (powerline-current-separator)
                                               (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (cdr powerline-default-separator-dir))))
               (lhs (list
                     ;; 変更の有無
                     (powerline-raw "[%*]" face0 'l)
                     ;; バッファサイズ
                     (when powerline-display-buffer-size
                       (powerline-buffer-size face0 'l))
                     ;; 文字エンコーディング(UUUなど)
                     (when powerline-display-mule-info
                       (powerline-raw mode-line-mule-info face0 'l))
                     ;; バッファID (ファイル名など)
                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                     (when (and (boundp 'which-func-mode)
                                which-func-mode)
                       (powerline-raw which-func-format face0 'l))

                     (powerline-raw " " face0)
                     (funcall separator-left face0 face1)

                     ;; メジャーモードを表示
                     (powerline-major-mode face1 'l)
                     (powerline-process face1)
                     ;; マイナーモードを表示
                     ;; (powerline-minor-modes face1 'l)

                     (powerline-narrow face1 'l)
                     (powerline-raw " " face1)
                     (funcall separator-left face1 face2)

                     ;; Gitのブランチ名などを表示
                     (powerline-vc face2 'r)
                     (when (bound-and-true-p nyan-mode)
                       (powerline-raw (list (nyan-create)) face2 'l))))

               (rhs (list
                     (powerline-raw global-mode-string face2 'r)
                     (funcall separator-right face2 face1)
                     ;; ポイントの行を表示
                     (powerline-raw "%4l行" face1 'l)
                     (powerline-raw ":" face1 'l)
                     ;; ポイントの桁を表示
                     (powerline-raw "%3c桁" face1 'r)

                     (funcall separator-right face1 face0)
                     (powerline-raw " " face0)
                     ;; 相対位置を%表示
                     ;; (powerline-raw "%6p" face0 'r)
                     (powerline-fill face0 0)
                     )))
          (concat (powerline-render lhs)
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs)))))))

  :config
  (powerline-my-theme)

  :defer-config
  (set-face-attribute 'mode-line           nil
                      :foreground "#fff" :background "#000000" :box     nil)
  (set-face-attribute 'powerline-active1   nil
                      :foreground "#fff" :background "#6699FF" :inherit 'mode-line)
  (set-face-attribute 'powerline-active2   nil
                      :foreground "#fff" :background "#3366FF" :inherit 'mode-line)
  (set-face-attribute 'mode-line-inactive  nil
                      :foreground "#444" :background "#ddd"    :box     nil)
  (set-face-attribute 'powerline-inactive1 nil
                      :foreground "#444" :background "#ccc"    :inherit 'mode-line)
  (set-face-attribute 'powerline-inactive2 nil
                      :foreground "#444" :background "#bbb"    :inherit 'mode-line)
  )


;; flycheck関連
(leaf flycheck
  :ensure t dash pkg-info
  :require let-alist seq
  :doc "On-the-fly syntax checking"
  :defvar (has-error-or-warnings)

  :advice
  ;; 上下のカーソル移動でc-modeのバッファを保存し、保存をトリガーにしたエラーチェックを行なう
  ;; (advice-add 'next-line :after #'save-current-c-mode-buffer-if-modified)
  (:after next-line     save-current-c-mode-buffer-if-modified)
  ;; (advice-add 'previous-line :after #'save-current-c-mode-buffer-if-modified)
  (:after previous-line save-current-c-mode-buffer-if-modified)

  :hook
  ;; エラーチェック直後に呼ばれるフック
  ;; (add-hook 'flycheck-after-syntax-check-hook 'change-header-color)
  ((flycheck-after-syntax-check-hook . change-header-color))

  :mode-hook
  (c-mode-common-hook . (
                         ;; flycheckによる文法チェックを行なう
                         (flycheck-mode t)
                         ;; エラー・警告を別バッファーに一覧表示する
                         (flycheck-list-errors)
                         ))
  
  :custom
  (
   ;; 前回のコンパイルの結果を保存する
   (has-error-or-warnings . nil)
   ;; 自動チェックするトリガーを指定(モード切り替え、保存、改行入力)
   (flycheck-check-syntax-automatically . '(mode-enabled save new-line)) ; 自動保存により idle-change は不要に
   ;; エラーを表示する関数をエコー領域を使わないものに変更
   (flycheck-display-errors-function . #'flycheck-display-error-messages-unless-error-list)

   )

  :setq-default
  (
   ;; 使わないチェッカー(未インストール: clang, cppcheck, 日本語未対応: c/c++gcc, 邪魔: emacs-lisp-checkdoc)
   (flycheck-disabled-checkers . '(c/c++-clang c/c++-cppcheck c/c++-gcc emacs-lisp-checkdoc))
  )

  :preface
  ;; 現在のバッファがc-modeで変更されているなら即座に保存する
  (defun save-current-c-mode-buffer-if-modified (&rest _ingored)
      "Immediately save current buffer if modified."
      (when (and (buffer-modified-p)
                 (eq major-mode 'c-mode)) 
        ;; (message "Saved (save-current-c-mode-buffer-if-modified)")
        (save-buffer)
        ))
  
  :config
  ;; flycheckを日本語化されたgccのエラーメッセージにも対応させる
  ;; flycheckにマクロの存在を教えるためにはeval-when-compileが必要
  (eval-when-compile 
   (defmacro flycheck-define-clike-checker (name command modes)
     `(flycheck-define-checker ,(intern (format "%s" name))
        ,(format "A %s checker using %s" name (car command)) ; quoted: (caadr command) orig: (car command)
        :command (,@command source-inplace)
        :error-patterns
        ((info
          line-start (or "<stdin>" (file-name))
          ":" line (optional ":" column)
          ": " (or "note" "備考") ": " (message) line-end)

         (warning
          line-start (or "<stdin>" (file-name))
          ":" line (optional ":" column)
          ": " (or "warning" "警告") ": " (message (one-or-more (not (any "\n["))))
          (optional "[" (id (one-or-more not-newline)) "]") line-end)

         ;; リンカの出すエラーにも対応
         (error
          line-start
          (or
           ;; 通常のエラーメッセージ
           (seq (or "<stdin>" (file-name)) ":" line (optional ":" column) ": "
                (seq  (or "fatal error" "致命的エラー" "error" "エラー")))
           ;; リンカのエラーメッセージ
           ;; 注意: line に数値をマッチさせないと flycheck-error-list に表示されない
           ;;       gccに-gオプションをつけて生成したオブジェクトなら行番号が表示されることが多い
           (seq (optional (one-or-more (not (any ":"))) "/ld: ") (file-name) ":" line (optional ":" column))
           )
          ": " (message) line-end)
         )
        :modes ',modes)
     ))

  ;; C言語用の日本語対応エラーチェッカー(c-gcc-ja, c-gcc-ja-with-ld)を定義
  ;; gccは -fsyntax-only ではチェックできないエラー・警告がある
  ;; さらにリンカまで実行しないとチェックできないエラーもある
  ;;   -O で初期化されていない自動変数をチェック可能
  ;;   -S でアセンブリファイル作成すると、いくつかのエラー・警告がチェック可能
  ;;   -o /dev/null でファイルは保存しないようにする
  ;;   -g でリンカのエラーメッセージにも行番号が付くことが多い
  (flycheck-define-clike-checker
   c-gcc-ja
   ;; 旧EDUのgcc 4では、色付けしない -fdiagnostics-plain-output オプションが使えない
   ;; ("gcc" "-fshow-column" "-Wall" "-Wextra" "-fdiagnostics-plain-output" "-std=gnu11" "-O" "-S" "-o" null-device)
   ("gcc" "-fshow-column" "-Wall" "-Wextra" "-std=gnu11" "-O" "-S" "-o" null-device)
   c-mode)

  ;; リンカまで実行するエラーチェッカー。倍時間がかかるため、コンパイル時に c-gcc-ja で検出できなかったときに切り替える
  (flycheck-define-clike-checker
   c-gcc-ja-with-ld
   ;; 旧EDUのgcc 4では、色付けしない -fdiagnostics-plain-output オプションが使えない
   ;; ("gcc" "-fshow-column" "-Wall" "-Wextra" "-fdiagnostics-plain-output" "-std=gnu11" "-g" "-O" "-o" null-device)
   ("gcc" "-fshow-column" "-Wall" "-Wextra" "-std=gnu11" "-g" "-O" "-o" null-device)
   c-mode)

  ;; c-gcc-jaのみチェッカーとして登録
  (add-to-list 'flycheck-checkers 'c-gcc-ja)

  (defun change-header-color ()
    ;; バッファが隠れている可能性もあるので必要
    (flycheck-list-errors)
    ;; エラー・警告の有無に応じてヘッダー行の背景色を変える
    (let ((header-line-background (if (flycheck-has-current-errors-p)
                                      (if (available-truecolor-p) "#ffaf00" "color-214")
                                    (if (available-truecolor-p) "#5fd787" "color-78"))))
      (with-current-buffer (get-buffer "*Flycheck errors*")
        ;; バッファが*Flycheck errors*に切り替わったので
        ;; flycheck-has-current-errors-pはnilになることに注意
        (face-remap-add-relative 'header-line
                                 :foreground (if (available-truecolor-p) "#1c1c1c" "color-234")
                                 :background header-line-background))
      )
    )
  
  :defer-config
  ;; flycheck-error-listのID欄の横幅を6->3に
  (aset flycheck-error-list-format 4 '("ID" 3 t))
  
  ;; エラー・警告・備考のフェイス設定
  (set-face-foreground 'flycheck-error "white")
  (set-face-background 'flycheck-error (if (available-truecolor-p) "red" "color-196"))
  (set-face-attribute 'flycheck-error nil :underline nil :weight 'bold)

  (set-face-foreground 'flycheck-warning "black")
  (set-face-background 'flycheck-warning (if (available-truecolor-p) "#ffaf00" "color-214"))
  (set-face-attribute 'flycheck-warning nil :underline nil :weight 'bold)

  (set-face-foreground 'flycheck-info "lightblue")


  ;; 指定したバッファのあるウィンドウの表示をflycheck-errorsに切り替える
  (defun switch-window-buffer-to-flycheck-errors-from (buf)
    (cond ((get-buffer "*Flycheck errors*")
           (set-window-buffer
            (get-buffer-window buf)
            (get-buffer "*Flycheck errors*"))
           )
          (t
           (delete-windows-on buf)))
    t)

  ;; flycheck-errorバッファにリストが含まれているか調べる
  (defun flycheck-errors-has-list-p ()
    (cond ((get-buffer "*Flycheck errors*")
           (save-current-buffer
             (set-buffer (get-buffer "*Flycheck errors*"))
             (save-excursion
               (if (eq (point-min) (point-max))
                   nil t))))
          (t
           nil)))
  
  )


;; beacon関連
(leaf beacon
  :ensure t
  :disabled nil
  :custom
  ((beacon-color              . "#aa3400")
   (beacon-size               . 64)
   (beacon-blink-when-focused . t)
   (beacon-blink-when-window-scrolls . nil)
   )
  :custom-face
  `((beacon-fallback-background . '((t (:background "#556b2f")))))
  :config
  (beacon-mode 1)
  )


;; display-buffer-alist関連
(leaf *display-buffer
  :doc "policy of buffer selection"
  :config
  ;; エラーを一覧表示するバッファー用の定義
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  ;; コンパイル結果を表示するバッファー用の定義
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*compilation*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  ;; シェルバッファー用の定義
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*shell*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))
  
  )


;; cc-mode関連
(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset c-cleanup-list)
  :commands (c-toggle-comment-style available-truecolor-p)
  :preface
  :defvar (my-header-line-format) 
  :custom-face
  :bind
  (c-mode-base-map
   ;; Enterキーによる挿入行を字下げする
   ("C-m" . c-context-line-break)
   )
  :mode-hook
  (c-mode-common-hook . (
                         (face-remap-add-relative 'header-line
                                                  :foreground (if (available-truecolor-p) "#1c1c1c" "color-234")
                                                  :background (if (available-truecolor-p) "#87afff" "color-111"))


                         ;; 字下げなどの形式を講義のプリントと同じ(K&R形式)にする
                         (c-set-style "k&r")
                         ;; 字下げの単位は4文字にする
                         (setq c-basic-offset 4)
                         ;; バックアップファイルを作らない
                         (setq-local make-backup-files nil)
                         ;; 字下げはスペースだけで行う(タブ文字を使わない)
                         (setq indent-tabs-mode nil)
                         ;; 「else」と「else if」は、直前の「}」と同じ行にくっつける
                         (setq c-cleanup-list
                               '(brace-else-brace
                                 brace-elseif-brace
                                 defun-close-semi)
                               )
                         ;; コマンドによるコメントは//を使う
                         (c-toggle-comment-style -1)

                         ;; 先頭をヘッダーラインにする
                         (setq-local my-header-line-format
                                     (concat "       "
                                             "F5:変換実行 F6:上下移動 F7:開く F8:表示切替 F9:再起動"
                                             (when (and (string= (getenv "TERM_PROGRAM") "WezTerm")
                                                        (not (window-system)))
                                               " (C|S|A-F10〜12:外見変更)")))
                         (setq header-line-format my-header-line-format)

                         ))
  )


;; smartparens関連
(leaf smartparens
  :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs"
  :ensure t dash
  :mode-hook
  (c-mode-common-hook . ((smartparens-mode t)
                         (turn-off-show-smartparens-mode)))
  (emacs-lisp-mode-hook . ((smartparens-mode t)
                           (smartparens-strict-mode t)
                           (turn-off-show-smartparens-mode)))

  :setq-default
  (
   ;; smartparensの()や{}内入力時の色付けをなくす
   (sp-highlight-pair-overlay . nil)
   )
  :require smartparens-config
  :advice-remove
  ;; (advice-remove 'delete-backward-char #'sp-delete-pair-advice)
  (delete-backward-char sp-delete-pair-advice)

  :commands (sp-forward-slurp-sexp sp-forward-barf-sexp sp-raise-sexp sp-splice-sexp sp-local-pair)
  :bind
  (smartparens-mode-map
   ;; smartparensのparedit風操作のキー割り当て(c-modeでは無効)
   ("C-<right>" . (lambda () (interactive) (unless (eq major-mode 'c-mode) (sp-forward-slurp-sexp))))
   ("C-<left>"  . (lambda () (interactive) (unless (eq major-mode 'c-mode) (sp-forward-barf-sexp))))
   ("M-r"       . (lambda () (interactive) (unless (eq major-mode 'c-mode) (sp-raise-sexp))))
   ("M-s"       . (lambda () (interactive) (unless (eq major-mode 'c-mode) (sp-splice-sexp))))
   ("C-<up>"    . sp-splice-sexp-killing-backward) ; lambdaで包むと動かない??
   ("M-<right>" . sp-select-next-thing)
   )

  :preface
  ;; {}入力後にEnterキーでインデントした空行を展開
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  :config
  (sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  ;; 表示はshow-paren-modeまかせ
  (show-smartparens-global-mode nil)

  )

;; real-auto-save関連
(leaf real-auto-save
  :doc "Automatically save your buffers/files at regular intervals"
  :ensure t
  :custom
  (((real-auto-save-interval . 2)))
  :hook
  (c-mode-common-hook . real-auto-save-mode)
  
  )


;; compile関連
(leaf compile
  :doc "run compiler as inferior of Emacs, parse error messages"
  :tag "builtin"
  :defvar (my-command my-compiled-source-buffer)
  :commands (compilation-buffer-has-warning-p flycheck-errors-has-list-p shell-and-insert indent-whole-file insert-my-command-at-end)
  
  :custom
  ;; 実行形式への変換に使うコマンドの確認(最下行に表示してEnterキー)をしない
  (compilation-read-command . nil)
  ;; 実行形式への変換前にソースファイルを(質問なしで)自動保存する
  (compilation-ask-about-save . nil)
  ;; 実行形式への変換後、最初のエラーにポイント(カーソル)を移動させる
  (compilation-auto-jump-to-first-error . t)
  ;; 直前に save-and-compile で実行形式に変換されたプログラムのコマンド名(先頭に"./"付き)
  (my-command . "")
  ;; 直前に save-and-compile で実行形式に変換されたプログラムのバッファ
  (my-compiled-source-buffer . nil)

  :config
  ;; compile終了時に実行する関数リストに追加
  (add-to-list 'compilation-finish-functions
               'switch-compilation-buffer)

  ;; 指定したバッファに警告があるか調べる
  (defun compilation-buffer-has-warning-p (buf)
    (save-current-buffer
      (set-buffer buf)
      (save-excursion
        (goto-char (point-min))
        (if (or (search-forward "warning:" nil t)
                (search-forward "警告:" nil t)) t nil))))

  ;; compilationバッファにエラー・警告があればflycheck-errors、なければshellに切り替え
  (defun switch-compilation-buffer (buf str)
    (cond ((or (string-match "abnormally" str)
             (compilation-buffer-has-warning-p buf))
         (setq has-error-or-warnings t)
         (message "エラー・警告を修正してください")
         ;; flycheck-errorsにリストがあればflycheck-errorsに切り換える
         (if (flycheck-errors-has-list-p)
             (switch-window-buffer-to-flycheck-errors-from "*compilation*")
           ;; flycheck-errorにリストがないなら、checkerをld付きのものに変更
           (add-to-list 'flycheck-checkers 'c-gcc-ja-with-ld)
           (delete 'c-gcc-ja flycheck-checkers)
             
           (switch-window-buffer-to-flycheck-errors-from "*compilation*")
           (with-current-buffer my-compiled-source-buffer
               (flycheck-buffer))
             )
         )
        (t
         (setq has-error-or-warnings nil)
         ;; flycheck-errorのcheckerをldなしのものに戻す
         (add-to-list 'flycheck-checkers 'c-gcc-ja)
         (delete 'c-gcc-ja-with-ld flycheck-checkers)
         
         (message "変換成功。実行できます")
         (shell-and-insert)
         )))

  ;; 現在のファイル全体をインデントする
  (defun indent-whole-file ()
    (interactive)
    (when (not (eq buffer-file-name nil))
      ;; ファイルバッファーにいる
      (let ((file-ext (file-name-extension buffer-file-name t))
            )
        (when (string= file-ext ".c")
          ;; Cのソースファイルのバッファーにいる
          (indent-region (point-min) (point-max))
          )))
    )
  
  ;; 編集中のC言語のソースファイルを実行形式に変換する
  (defun save-and-compile ()
    (interactive)
    (cond ((or (eq buffer-file-name nil)
               (not (string= (file-name-extension buffer-file-name t) ".c")))
           ;; 現在のバッファーはC言語のソースファイルではない
           (when (string= (buffer-name) "*shell*")
             ;; shellにいるならコマンド名を挿入
             (shell-and-insert))
           )
          ;; 現在のバッファーはC言語のソースファイル
          (t
           ;; コンパイル対象のバッファ名
           (setq my-compiled-source-buffer (buffer-name))
           ;; ファイル全体のインデントを整える
           (indent-whole-file)
           ;; (変更されていれば)問答無用で保存
           (save-buffer)
           ;; flycheckによるエラーチェックの結果を更新
           (when (flycheck-mode)
             ;; ウィンドウ分割していなくても、チェック後に自動分割される
             ;; flycheck-after-syntax-check-hook を参照
             (flycheck-buffer)
             ;; flycheckのエラーチェック終了まで待つ
             (while (flycheck-running-p)
               (sit-for 0.125))
             )

           (let
               ;; TODO: カレントディレクトリが~/でないときの処理
               ((file-base (shell-quote-argument (file-name-base buffer-file-name)))
                (file-name (shell-quote-argument (file-name-nondirectory buffer-file-name))))
             ;; 後で実行するときのコマンド名
             (setq my-command (format "./%s" file-base))
             ;; コンパイルコマンドにファイル名などを埋め込む
             (set (make-local-variable 'compile-command)
                  (format "gcc -Wall -Wextra -std=gnu11 -lm -g -O -o%s %s" file-base file-name))
             ;; コンパイルを実行。画面が自動分割されて変換結果が表示される
             ;; コンパイル完了後はswitch-compilation-bufferが呼び出されてflycheck-errors-listに切り替わる
             (compile (eval compile-command))
            
             )
           )
          )
    )

  ;; 現在のポイントにコマンド名を挿入
  (defun insert-my-command-at-end ()
    ;; バッファー末尾に移動して
    (goto-char (point-max))
    (cond
     ;; ポイントより前にコマンド名の長さ分の文字列がなければ
     ((< (point) (length my-command))
      ;; そのままコマンド名を挿入する
      (insert my-command))
     ;; 以下、コマンド名の多重挿入を避けるための処理
     (t
      ;; ポイント直前のコマンド名の長さ分の文字列を切り出し
      (let ((prev-str (buffer-substring-no-properties
                       (point) (- (point) (length my-command))))
            )
        ;; ポイント直前の文字列がコマンド名と同じでなければ
        (when (not (string= prev-str my-command))
          ;; コマンド名を挿入する
          (insert my-command))
        )
      )
     )
    )

  ;; 実行画面(*shell*バッファー)の末尾にコマンド名を挿入
  (defun shell-and-insert ()
    (interactive)
    (cond
     ((eq buffer-file-name nil)
      ;; ファイルバッファー以外にいる
      (shell)
      ;; Emacs終了時にこのシェルが実行中でも問い合わせしない
      (set-process-query-on-exit-flag (get-process "shell") nil)
      (insert-my-command-at-end)
      )
     (t
      ;; ファイルバッファーにいる
      (let ((file-ext (file-name-extension buffer-file-name t))
            (file-base (shell-quote-argument (file-name-base buffer-file-name)))
            )
        (when (string= file-ext ".c")
          ;; Cのソースファイルのバッファーにいる
          ;; 実行形式に変換後のコマンド名を設定
          (setq my-command (format "./%s" file-base))
          ;; 画面分割されていなければ、分割する
          (when (one-window-p)
            (split-window))
          ;; 別ウィンドウにポインター(カーソル)を移す
          (other-window 1)
          ;; シェルを実行中でなければ実行し、現在のウィンドウに表示
          ;; すでに実行中なら現在のウィンドウに表示(バッファー切り替え)
          (shell)
          ;; Emacs終了時にこのシェルが実行中でも問い合わせしない
          (set-process-query-on-exit-flag (get-process "shell") nil)
          ;; バッファー末尾にコマンド名を挿入
          (insert-my-command-at-end))))
     )
    )


  
  )


;; autoinsert関連
(leaf autoinsert
  :doc "automatic mode-dependent insertion of text into new files"
  :tag "builtin"
  :pre-setq
  :custom
  (
   ;; テンプレートを使うかどうかの質問をしない(常に使う)
   (auto-insert-query . nil)
   ;; テンプレートのリストをクリア
   (auto-insert-alist . nil)

   )
  :config
  ;; ファイルを新規作成する際にテンプレートを利用する
  (add-to-list 'find-file-not-found-functions
               #'(lambda ()
                   (auto-insert)
                   ;; auto-insert 後に変更フラグを付ける
                   (set-buffer-modified-p t)
                   ))

  
  ;; C言語用テンプレートを設定
  (define-auto-insert
    '("\\.c$" . "C template")
    [("C template: "
      (c-mode)			; テンプレート挿入時はc-modeになっていないため必要
      "#include <stdio.h>" \n \n	; 先頭行と次行(空行)を挿入
      "int main(void)" \n	; mainの引数はvoidにする
      "{" > \n			; {の位置を行頭にするには後ろに>を入れてインデント
      > _ \n			; インデント後にポインターを置く
      "return 0;" \n		; 前行と同じインデントでreturn文を置く
      "}" > \n )])		; {と位置を合わせるには後ろに>を入れてインデント

  ;; .bash_profile用テンプレートを設定
  (define-auto-insert
    '("/\\.bash_profile$" . "Bash profile template")
    [("Bash profile template: "
      "export LANG=ja_JP.UTF-8\n"
      "if [[ ! $PATH = */snap/emacs/current/usr/bin* ]] ; then\n"
      "\texport PATH=/snap/emacs/current/usr/bin:$PATH\n"
      "fi\n"
      "export HISTCONTROL=ignoreboth\n"
      "source ~/.bashrc\n"
      "printf \"\\033]1337;SetUserVar=%s=%s\\007\" \"REMOTE_HOST_NAME\" $(hostname | base64)\n"
      "printf \"\\033]1337;SetUserVar=%s=%s\\007\" \"REMOTE_HOST_PRETTY_NAME\" $(sed -nre 's/^PRETTY_NAME=\"([^\(]+) .*\"/\\1/p' /etc/os-release | base64)\n"
      "if [[ -n \"$SSH_CONNECTION\" ]]; then\n"
      "\tprintf \"\\033]1337;SetUserVar=%s=%s\\007\" \"REMOTE_HOST_PORT\" $(echo -n $SSH_CONNECTION | cut -d' ' -f 4 | base64)\n"
      "fi\n"
      )])
  )


;; highlight-defined関連 開発時のみ
(leaf highlight-defined
  :doc "Syntax highlighting of known Elisp symbols"
  :emacs>= 24
  :ensure t
  :disabled (not in-development)
  :hook
  ((emacs-lisp-mode-hook . highlight-defined-mode))
  )

;; ivy/swiper/counsel関連 開発時のみ
(leaf ivy
  :doc "Incremental Vertical completYon"
  :tag "matching"
  :emacs>= 24.5
  :ensure t
  :disabled (not in-development)
  
  :config
  (leaf swiper
    :doc "Isearch with an overview.  Oh, man!"
    :ensure t
    :require ivy
    :emacs>= 24.5
    :bind
    (("C-s" . swiper))
    )

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :require ivy swiper
    :emacs>= 24.5
    :ensure t
    :bind
    (
     ("M-x" . counsel-M-x)
     ("M-i" . counsel-imenu)
     ("C-x C-b" . counsel-ibuffer)
     )

    )
  
  )

(leaf bm
  :doc "Visible bookmarks in buffer."
  :ensure t
  :config
  (global-set-key (kbd "<f2>") 'bm-next)
  (global-set-key (kbd "S-<f2>") 'bm-toggle)
  :defer-config
  (set-face-attribute 'bm-face nil
                      :foreground "#101010"
                      :background "#608060"
                      )

  )


;;
;; 再起動関連
;;
(leaf restart-emacs
  :doc "restart emacs from within emacs"
  :ensure t
  :require desktop
  :defun (restart-emacs--get-emacs-binary restart-emacs--string-join)
  :advice
  ;; (advice-add 'restart-emacs--start-emacs-in-terminal :override #'my-restart-emacs)
  (:override restart-emacs--start-emacs-in-terminal my-restart-emacs)

  :preface
  ;; 先頭に空白を入れることでコマンド履歴に残さない
  ;; 環境変数 HISTCONTROL に「ignoreboth」または「ignorespace」を設定する必要がある
  (defun my-restart-emacs (&optional args)
    "Start Emacs in current terminal (modified)."
    (suspend-emacs (format " fg ; clear ; %s %s -nw"
                           (shell-quote-argument (restart-emacs--get-emacs-binary))
                           (restart-emacs--string-join (mapcar #'shell-quote-argument
                                                               args)
                                                       " "))))

  ;; 現在開いているバッファの状態を保存してEmacsを再起動する
  (defun desktop-save-and-restart-emacs ()
    "Run desktop-save and restart-emacs."
    (interactive)
    (desktop-save user-emacs-directory) ; "~"
    (restart-emacs)
    )
  
  :config
  ;; emacs-restartによる再起動後なら、開いていたバッファを再現
  (when (file-exists-p (locate-user-emacs-file ".emacs.desktop")) ; "~/.emacs.desktop"
    (desktop-read user-emacs-directory)                                               ; "~"
    (delete-file (locate-user-emacs-file ".emacs.desktop")) ; "~/.emacs.desktop"
    (message "Emacsを再起動してバッファを復元しました"))

  )
