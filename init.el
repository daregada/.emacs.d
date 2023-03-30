;;; init.el --- Emacs init file for EDU system (logos). -*- lexical-binding: t; -*-

;;; Commentary:

;; Edu(logos)用のinit.el
;; ~/.emacs.dに置くこと

;;; Code:

;; 配布時にはコメントにすること
;; (setq debug-on-error t)

;; Emacs 28.1以降のNative Compilationの警告を抑制
(unless (version< emacs-version "28.1")
  (custom-set-variables '(native-comp-async-report-warnings-errors 'silent)))

;; Emacs 28で、全角スペース"　"などがnobreak-spaceに追加された
;; それらすべての特殊表示を無効にする
(unless (version< emacs-version "28.0")
  (setq nobreak-char-display nil))

;; カスタムファイルの指定。カスタム設定が custom.el に分離されるようになる
(setq custom-file (if (boundp 'user-emacs-directory)
                      (expand-file-name "custom.el" user-emacs-directory)
                    "~/.emacs.d/custom.el"))

;; 保存されたカスタムファイルを(わざと)読み込まない
;; (if (file-exists-p (expand-file-name custom-file))
;;     (load-file (expand-file-name custom-file)))

;; 以下のコードで必要
(require 'cl-lib)

;;
;; 画面表示関連
;;
;; メニューバーなどを表示しない
(menu-bar-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Emacsのスタート画面を消す
(setq inhibit-startup-screen t)

;; 起動時にscratchバッファー先頭のメッセージを出さない
(setq initial-scratch-message nil)

;; カッコの対応を表示しない
(show-paren-mode nil)
;; カッコのマッチ部分の背景色を、全体の背景色に応じて変える
(defface my-paren-match-remap-style
  '((((background dark))  (:background "color-239"))
    (t                    (:background "color-254")))
  "test"
  )

;; 閉じカッコ入力時のハイライト表示をしない
(setq blink-matching-paren nil)

;; rawバイトは16進数で表示(Emacs 26.1以降で有効)
(unless (version< emacs-version "26.1")
  (setq display-raw-bytes-as-hex t))

;; リージョンに上書き
(delete-selection-mode t)

;; マウスでポイントの位置変更や範囲コピー、スクロールを可能に
(xterm-mouse-mode t)
(mouse-wheel-mode t)
;; ホイールでスクロール
(global-set-key [mouse-4] #'(lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] #'(lambda () (interactive) (scroll-up   1)))
;; 右・中クリックでリージョンをコピー
(global-set-key [mouse-2] #'copy-region-as-kill)
(global-set-key [mouse-3] #'copy-region-as-kill)


;;
;; whitespace関連
;;
(when (require 'whitespace nil t)
  ;; nobreak-spaceとtabの外見をwhitespaceで変更する
  (setq whitespace-style '(face           ; faceで可視化
                           ;; trailing    ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           ;; empty       ; 先頭/末尾の空行
                           ;; space-mark  ; 空白表示のマッピング
                           ;; tab-mark    ; タブ表示のマッピング
                           ))
  ;; nobreak-spaceのみ対象とする
  (setq whitespace-space-regexp "\\([\u00a0\u1680\u2001-\u200a\u202f\u205f\u3000]+\\)")

  ;; 色付けしたい空白系文字のface
  (set-face-attribute 'whitespace-space nil
                      :background "pink"
                      :foreground "black"
                      :weight 'bold)

  ;; 色付けしたいタブ系文字のface
  (set-face-attribute 'whitespace-tab nil
                      :background "lightgray"
                      :foreground "red"
                      :underline t)
  ;; 　   　 sample 		
  )

;;
;; キーカスタマイズ関連
;;
;; Ctrl-Vに「貼り付け」(yank)を割り当て(Windwosユーザー向け対策)
;; Ctrl-Vの元の機能「1画面下を表示」(scroll-up-command)はPagedownで代用可能
(global-set-key (kbd "C-v") 'yank)

;; Ctrl-Zに「元に戻す」(undo)を割り当て(Windowsユーザー向け対策)
;; Ctrl-Zの元の機能「一時停止」(suspend-emacs)は「M-x suspend-emacs」で代用可能
(global-set-key (kbd "C-z") 'undo)

;; F5キーとCtrl-C cとCtrl-C 5に、「編集中のプログラムを実行形式に変換する機能」を割り当て
;; エラーが発生した場合は、下のウィンドウがflycheck-errorsの表示に戻り
;; 変換に成功した場合は、下のウィンドウがshellの表示に切り替わってコマンド名が挿入される
(global-set-key (kbd "<f5>") 'save-and-compile)
(global-set-key (kbd "C-c c") 'save-and-compile)
(global-set-key (kbd "C-c 5") 'save-and-compile)

;; F6キーとCtrl-C sとCtrl-C 6に、「別ウィンドウ(または別バッファー)の選択」を割り当て
;; 画面が分割表示されているときは、ポインター(カーソル)が別ウィンドウに移る
;; (shellバッファーのウィンドウから移った場合は下のウィンドウがflycheck-errorsの表示に戻る)
;; 画面が分割表示されていないときは、表示内容が別のバッファー(ファイルや実行画面)に切り替わる
(global-set-key (kbd "<f6>") 'other-window-or-buffer)
(global-set-key (kbd "C-c s") 'other-window-or-buffer)
(global-set-key (kbd "C-c 6") 'other-window-or-buffer)

;; F7キーとCtrl-C fとCtrl-C 7に、「指定した番号の演習用ファイル(progXX.c)を開く」を割り当て
(global-set-key (kbd "<f7>") 'find-practice-file)
(global-set-key (kbd "C-c f") 'find-practice-file)
(global-set-key (kbd "C-c 7") 'find-practice-file)

(when (fboundp 'display-line-numbers-mode)
  ;; F8キーとCtrl-C nとCtrl-C 8に、「行番号表示の切り替え」を割り当て
  (global-set-key (kbd "<f8>") 'display-line-numbers-mode)
  (global-set-key (kbd "C-c n") 'display-line-numbers-mode)
  (global-set-key (kbd "C-c 8") 'display-line-numbers-mode)
  ;; 行番号の見た目を変える
  (setq-default display-line-numbers-width 5)
  (set-face-attribute 'line-number nil
                      :foreground "gray"
                      :background "silver")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "black"
                      :background "gray")
  ;;(set-face-attribute 'line-number nil
  ;;                    :foreground "brightwhite"
  ;;                    :background "gray")
  ;; (set-face-attribute 'line-number-current-line nil
  ;;                     :foreground "brightwhite"
  ;;                     :background "black")
  )

;; Insertキーの無効化。間違って押して戻せない人が多いため
(global-set-key (kbd "<insertchar>")
                #'(lambda ()
                   (interactive)
                   (message "Insertキーは無効です。上書きモードは M-x overwrite-mode で。")))

;; CentOS7のgnome-terminal用(Ctrl-Jが入力できないため)
(global-set-key (kbd "<f10>") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "<f10>") 'eval-print-last-sexp)


;; ヘッダーラインの見た目を変える
(set-face-attribute 'header-line nil
                    :foreground "black"
                    :background "gray"
                    :inherit nil
                    :overline nil
                    :underline t)


;; 軽量バッファーモードの定義
(defun lightweight-buffer-mode ()
  "Lightweight Buffer Mode for novice."
  (interactive)
  (setq mode-name "Lightweight Buffer")
  (setq major-mode 'lightweight-buffer-mode)
  (if (fboundp 'show-paren-local-mode)
      (show-paren-local-mode nil)
    (setq-local show-paren-mode nil))
  (run-hooks 'lightweight-buffer-mode-hook))

(with-current-buffer (get-buffer "*scratch*")
  ;; 起動時のscratchバッファーをlightweight-buffer-modeに
  (setq initial-major-mode 'lightweight-buffer-mode)
  ;; 先頭行をヘッダーラインに
  (setq header-line-format
        (concat " 注意: ここにC言語のプログラムを書かないでください。"))
  (insert "\nC言語のソースファイル(拡張子.c)を作成/開いて、そこに書いてください。\n\n"))

;;
;; 起動処理を完了後
;;
(if (fboundp 'advice-add)
    (advice-add 'command-line-1 :after #'display-notes)
  (with-current-buffer (get-buffer "*scratch*")
    (insert "Emacsのバージョンが古くて動きません。\n"
            "新しいEmacsを優先起動する設定の.bash_profileを作っていないのでは。\n\n")
    )
  )

(defun display-notes (&rest _ignored)
  ;; *scratch*に起動後の注意書きを表示
  "Display notes for novice user."
  
  (with-current-buffer (get-buffer "*scratch*")
    (when (not (one-window-p))
      (delete-windows-on (get-buffer "*Compile-Log*")))
    (goto-char (point-max))
    ;; 注意書きを挿入
    (insert "ファイルを作成/開くには、キー操作(C-x C-f)を使います。\n"
            "(「C-x」はCtrlキーを押したままXキーを押す操作。その後の「C-f」も同様)\n"
            "一番下に「Find File:」と出たら、ファイル名を入力しEnterキーを押してください。\n\n"
            "ヒント: 演習用のファイル(progNN.c)であれば、もっと簡単な方法があります。\n"
            "「<f7>」(F7キー)を押し、一番下に「Program Number:」と出たら、\n"
            "プログラム番号(1桁か2桁の整数)を入力し、Enterキーを押してください。\n\n"
    )
    (insert "起動時の処理が完了しました。\n")
    (set-buffer-modified-p nil)
    (face-remap-add-relative 'header-line
                             :background "orange")

))

;;
;; package.el関連
;;
(require 'package)

;; MELPAに限定
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; 初期化
(package-initialize)

;; 必須パッケージ
(defvar my-favorite-packages
  '(
    flycheck
    powerline
    real-auto-save
    smartparens
    ))

;; 開発用パッケージ
(defvar my-development-packages
  '(
    swiper
    counsel
    highlight-defined
    bm
    package-utils
    ))

(defun all-packages-installed-p (package-list)
  "Check if all favorite packages are installed.

PACKAGE-LIST: list of packages."
  (interactive)
  (catch 'early-return-in-all-packages-installed-p
    (dolist (package package-list)
      (unless (package-installed-p package)
        (throw 'early-return-in-all-packages-installed-p nil)))
    t))

;;
;; 引数で指定したリスト内のパッケージがインストールされていなければ自動的にインストール
;;
(defun install-listed-packages-automatically (package-list &optional verbose)
  "Install listed packages automatically.

PACKAGE-LIST: list of packages.
VERBOSE: insert messages to *scratch* if non-nil.
"
  (with-current-buffer (get-buffer "*scratch*")
    (when verbose
      (insert "パッケージの自動導入作業を行ないます。\n"
              "導入作業が終わるまでしばらく時間がかかります。\n"
              "最上行がオレンジ色になるまでお待ちください。\n\n"))
    (unless package--initialized
      (when verbose 
        (insert "パッケージシステムの初期化中。\n")
        (redisplay))
      (package-initialize))
    (unless package-archive-contents
      (when verbose 
        (insert "パッケージの一覧を取得中。\n")
        (redisplay))
      (package-refresh-contents))
    (dolist (package package-list)
      (unless (package-installed-p package)
        (when verbose 
          (insert (symbol-name package) " パッケージ導入中。\n")
          (redisplay))
        (package-install package)))

    (when verbose 
      (insert "\nパッケージ自動導入処理が完了しました。\n\n")
      (redisplay))

    )
  
  )

;; 必須パッケージを自動インストール
(unless (all-packages-installed-p my-favorite-packages)
  (install-listed-packages-automatically my-favorite-packages t)
  )

;; 開発用パッケージを自動インストールする関数
(defun install-development-packages-automatically ()
  "Install development packages."
  (interactive)
  (unless (all-packages-installed-p my-development-packages)
    (install-listed-packages-automatically my-development-packages nil)
    )
  )


;;
;; shell関連
;;
(when (require 'shell)
  ;; プロンプトは読み取り専用
  (custom-set-variables '(comint-prompt-read-only t))
  ;; プロンプトにコマンドの戻り値($?)を含める
  (setenv "PS1" "[\\u@\\h \\W ($(echo $?;))]\\$ ")
  )

;;
;; which-function-mode関連
;;
;; ファイル名はwhich-func.el
(when (require 'which-func nil t)
  (set-face-foreground 'which-func "orange")
  (custom-set-variables '(which-func-unknown "外部"))
  ;; which-func-modes に設定したモードのみ有効になる
  (custom-set-variables '(which-func-modes '(c-mode emacs-lisp-mode)))
  ;; グローバルに有効化(which-func-modes 参照)
  (which-function-mode t)
  )

;;
;; powerline関連
;;
(when (require 'powerline nil t)
  (custom-set-variables '(powerline-display-buffer-size nil)
                        '(powerline-display-mule-info nil))

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

  (when (fboundp 'powerline-my-theme)
    (powerline-my-theme)))


;;
;; flycheck関連
;;
(when (require 'flycheck)
  ;; flycheck-error-listのID欄の横幅を6->3に
  (aset flycheck-error-list-format 4 '("ID" 3 t))

  ;; チェックに使わないチェッカーの既定値
  ;; インストールされていない clang, cppcheck
  ;; 日本語未対応の c/c++-gcc, デバッグ時に邪魔な emacs-lisp-checkdoc
  (setq-default flycheck-disabled-checkers '(c/c++-clang
                                             c/c++-cppcheck
                                             c/c++-gcc
                                             emacs-lisp-checkdoc))

  ;; flycheckを日本語化されたgccのエラーメッセージにも対応させる
  (defmacro flycheck-define-clike-checker (name command modes)
    `(flycheck-define-checker ,(intern (format "%s" name))
       ,(format "A %s checker using %s" name (car command))
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
          ;;       gccに-gオプションをつけて生成したオブジェクトなら行番号が表示される
          (seq (optional (one-or-more (not (any ":"))) "/ld: ") (file-name) ":" line (optional ":" column))
          )
	 ": " (message) line-end)
	)
       :modes ',modes)
    )

  ;; C言語用の日本語対応エラーチェッカー(c-gcc-ja)を定義
  ;; gccは -fsyntax-only ではチェックできないエラー・警告がある
  ;; -O で初期化されていない自動変数をチェック可能になる
  ;; -S でアセンブリファイルまで作成すると、いくつかのエラー・警告がチェック可能になる
  ;; -o /dev/null でファイルは保存しないようにする
  (flycheck-define-clike-checker
   c-gcc-ja
   ;; 旧EDUのgcc 4.8.5では -fdiagnostics-plain-output オプションが使えない
   ;; ("gcc" "-fshow-column" "-Wall" "-Wextra" "-fdiagnostics-plain-output" "-std=gnu11" "-O" "-S" "-o" null-device)
   ("gcc" "-fshow-column" "-Wall" "-Wextra" "-std=gnu11" "-O" "-S" "-o" null-device)
   c-mode)

  ;; C言語用の日本語対応エラーチェッカー(c-gcc-ja-with-ld)を定義
  ;; リンカまで実行しないとチェックできないエラーがある
  ;; -g でリンカのエラーメッセージにも行番号が付く
  ;; -o /dev/null でファイルは保存しないようにする
  ;; コストを考えて、コンパイル時に c-gcc-ja では検出できなかったときだけ切り替えて使う
  (flycheck-define-clike-checker
   c-gcc-ja-with-ld
   ;; 旧EDUのgcc 4.8.5では -fdiagnostics-plain-output オプションが使えない
   ;; ("gcc" "-fshow-column" "-Wall" "-Wextra" "-fdiagnostics-plain-output" "-std=gnu11" "-g" "-O" "-o" null-device)
   ("gcc" "-fshow-column" "-Wall" "-Wextra" "-std=gnu11" "-g" "-O" "-o" null-device)
   c-mode)


  ;; c-gcc-jaのみチェッカーとして登録
  (add-to-list 'flycheck-checkers 'c-gcc-ja)

  ;; エラーを表示する関数をエコー領域を使わないものに変更
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  ;; 自動チェックするトリガーを指定
  (setq flycheck-check-syntax-automatically
	'(
          mode-enabled
          save                            ; 自動保存にも連動
          new-line                        ; auto-completeと併用するときは無効にすること
          ;; idle-change
          ))

  (advice-add 'next-line :after #'save-current-c-mode-buffer-if-modified)
  (advice-add 'previous-line :after #'save-current-c-mode-buffer-if-modified)

  ;; カレントバッファーのCソースコードが変更されていたら保存する
  (defun save-current-c-mode-buffer-if-modified (&rest _ingored)
    "Immediately save current buffer if modified."
    (when (and (buffer-modified-p)
               (eq major-mode 'c-mode)) 
      (message "saved (next-line-after)")
      (save-buffer)
      ))

  ;; エラー・警告・備考のフェイス設定
  (set-face-foreground 'flycheck-error "white")
  (set-face-background 'flycheck-error "red")
  (set-face-attribute 'flycheck-error nil :underline nil :weight 'bold)

  (set-face-foreground 'flycheck-warning "black")
  (set-face-background 'flycheck-warning "orange")
  (set-face-attribute 'flycheck-warning nil :underline nil :weight 'bold)

  (set-face-foreground 'flycheck-info "lightblue")

  ;; 前回のコンパイルの結果を保存する
  (defvar has-error-or-warnings nil)

  ;; エラーチェック直後に呼ばれるフック
  (add-hook 'flycheck-after-syntax-check-hook
            'change-header-color)

  (defun change-header-color () 
    ;; バッファが隠れている可能性もあるので必要
    (flycheck-list-errors)
    ;; エラー・警告の有無に応じてヘッダー行の背景色を変える
    (let ((header-line-background (if (flycheck-has-current-errors-p)
                                      "orange" "lightgreen")))
      (with-current-buffer (get-buffer "*Flycheck errors*")
	;; バッファが*Flycheck errors*に切り替わったので
	;; flycheck-has-current-errors-pはnilになることに注意
	(face-remap-add-relative 'header-line
				 :background header-line-background))
      )
    )
  )

;;
;; display-buffer-alist関連
;;
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


;;
;; C言語のソースファイルを表示するバッファー限定の設定
;;
(require 'cc-mode)
;; autoinsertのために(c-mode-common-hookでではなく)ここで設定すること
;; 字下げなどの形式を講義のプリントと同じ(K&R形式)にする
;; 字下げの単位は4文字にする
(setq-default c-default-style "k&r"
              c-basic-offset 4)

;;
;; smartparens関連
;;
(when (require 'smartparens nil t)
  (require 'smartparens-config)

  ;; {}入力後にEnterキーでインデントした空行を展開
  (declare-function sp-local-pair "smartparens" (p1 p2 p3 p4 p5))
  (sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; smartparensの()や{}内入力時の色付けをなくす
  (setq-default sp-highlight-pair-overlay nil)

  ;; smartparensのparedit風操作のキー割り当て
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-r") 'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-<up>") 'sp-splice-sexp-killing-backward)

  ;; smartparensのカッコ削除用アドバイスを除去
  (declare-function sp-delete-pair-advice "smartparens" nil)
  (advice-remove 'delete-backward-char #'sp-delete-pair-advice)
  ;; smartparensの色付けを無効にする
  (show-smartparens-global-mode nil)

  )

;; フック
;; TODO: モードごとに分けて記述
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;;
   ;; show-paren-mode関連
   ;;
   ;; c-mode限定で対応するカッコの強調を行なう
   (if (fboundp 'show-paren-local-mode)
       (show-paren-local-mode t)
     (setq-local show-paren-mode t))

   ;; 対応するカッコのみを強調
   (setq-local show-paren-style 'parenthesis)

   ;; 全角スペース"　"などの外見を変更
   (face-remap-add-relative 'nobreak-space
                    :foreground "black"
                    :background "pink"
                    :underline  nil)

   ;;
   ;; display-line-numbers-mode関連
   ;;
   (when (fboundp 'display-line-numbers-mode)
     (display-line-numbers-mode t))

   ;;
   ;; c-mode (cc-mode)関連
   ;;
   ;; 字下げはスペースだけで行う(タブ文字を使わない)
   (setq indent-tabs-mode nil)
   ;; Enterキーによる挿入行を字下げする
   (define-key c-mode-base-map "\C-m" 'c-context-line-break)
   ;; BackspaceキーやCtrl-Dキーで複数のスペース・改行を一括削除し、
   ;; (c-toggle-hungry-state 1)
   ;; 「;」や「}」を入力すると自動的に改行する
   ;; (c-toggle-auto-newline 1)
   ;; 「else」と「else if」は、直前の「}」と同じ行にくっつける
   (setq c-cleanup-list
         '(brace-else-brace
           brace-elseif-brace
           defun-close-semi)
         )
   ;; コマンドによるコメントは//を使う
   (c-toggle-comment-style -1)

   ;;
   ;; compile関連
   ;;
   (custom-set-variables
    ;; 実行形式への変換に使うコマンドの確認(最下行に表示してEnterキー)をしない
    '(compilation-read-command nil)
    ;; 実行形式への変換前にソースファイルを(質問なしで)自動保存する
    '(compilation-ask-about-save nil)
    ;; 実行形式への変換後、最初のエラーにポイント(カーソル)を移動させる
    '(compilation-auto-jump-to-first-error t)
    )

   ;; 実行形式への変換後、エラー(と思われる)範囲をハイライト表示する秒数
   ;; flycheck が同様の処理をするので設定しない
   ;; (setq next-error-highlight 5)

   ;;
   ;; real-auto-save関連
   ;;
   (when (require 'real-auto-save nil t)
     (real-auto-save-mode)
     (custom-set-variables '(real-auto-save-interval 2))
     )

   ;;
   ;; flycheck関連
   ;;
   ;; flycheckによる文法チェックを行なう
   (flycheck-mode t)
   ;; エラー・警告を別バッファーに一覧表示する
   (flycheck-list-errors)

   ;;
   ;; smartparens関連
   ;;
   ;; smartparensによるカッコ組入力を行なう
   (smartparens-mode t)
   ;; smartparensによる色付けを行わない
   (turn-off-show-smartparens-mode)

   ;; 全角スペース"　"などの外見を変更
   (when (fboundp 'whitespace-mode)
     (whitespace-mode t))

   ;; バックアップファイルを作らない
   ;;
   (setq-local make-backup-files nil)
   ))

;;
;; autoinsert関連
;;
(when (require 'autoinsert nil t)
  ;; ファイルを新規作成する際にテンプレートを利用する
  (add-to-list 'find-file-not-found-functions
               #'(lambda ()
                   (auto-insert)
                   ;; auto-insert 後に変更フラグを付ける
                   (set-buffer-modified-p t)
                   ))

  ;; テンプレートを使うかどうかの質問をしない(常に使う)
  (custom-set-variables '(auto-insert-query nil))

  ;; テンプレートのリストをクリア
  (custom-set-variables '(auto-insert-alist nil))

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
      "export PATH=/snap/emacs/current/usr/bin:$PATH\n"
      "source ~/.bashrc\n"
      )])

  )

;; 
;; 自作コマンドと関数
;; 

;; 直前に save-and-compile で実行形式に変換されたプログラムのコマンド名(先頭に"./"付き)
(defvar my-command "")
;; 直前に save-and-compile で実行形式に変換されたプログラムのバッファ
(defvar my-compiled-source-buffer nil)

;;
;; 編集中のC言語のソースファイルを実行形式に変換する
;;
(defun save-and-compile ()
  (interactive)
  (cond ((or (eq buffer-file-name nil)
             (not (string= (file-name-extension buffer-file-name t) ".c")))
         ;; 現在のバッファーはC言語のソースファイルではない

         (when (string= (buffer-name) "*shell*")
           ;; shellにいるならコマンド名を挿入
           (shell-and-insert))
         )
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
             ((file-base (shell-quote-argument (file-name-base buffer-file-name)))
              (file-name (shell-quote-argument (file-name-nondirectory buffer-file-name))))
           ;; 後で実行するときのコマンド名
           (setq my-command (format "./%s" file-base))
           ;; コンパイルコマンドにファイル名などを埋め込む
           (set (make-local-variable 'compile-command)
                (format "gcc -Wall -Wextra -std=gnu11 -lm -g -O -o%s %s"
                        file-base
                        file-name
                        ))
           ;; コンパイルを実行。画面が自動分割されて変換結果が表示される
           ;; コンパイル完了後はswitch-compilation-bufferが呼び出されてflycheck-errors-listに切り替わる
           (compile (eval compile-command))

           
           ))))

;; 
;; 別ウィンドウ(または別バッファー)の選択
;;
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

;;
;; マウス操作で*shell*や*compilation*バッファからソースコードに切り替えた時に*Flycheck errors*を表示
;;
(advice-add 'mouse-set-point :before #'mouse-set-point-before)
(advice-add 'mouse-set-point :after #'mouse-set-point-after)

(defvar my-prev-buffer-name)

(defun mouse-set-point-before (&rest _ignored)
  (setq my-prev-buffer-name (buffer-name)))

(defun mouse-set-point-after (&rest _ignored)
  (unless (string= my-prev-buffer-name (buffer-name))
    ;; (message (concat "mouse-set-pointアドバイス実行中: " my-prev-buffer-name "->" (buffer-name)))
    (when (and (or (string= my-prev-buffer-name "*shell*")
                   (string= my-prev-buffer-name "*compilation*"))
               (and (not (eq buffer-file-name nil))
                    (string= (file-name-extension buffer-file-name t) ".c")))
      (switch-window-buffer-to-flycheck-errors-from my-prev-buffer-name))))

;;
;; 現在のポイントにコマンド名を挿入
;;
(defun insert-my-command-at-end ()
  ;; バッファー末尾に移動して
  (goto-char (point-max))
  ;; ポイントより前にコマンド名の長さ分の文字列がなければ
  (cond
   ((< (point) (length my-command))
    ;; コマンド名を挿入する
    (insert my-command))
   (t
    ;; 以下、コマンド名の多重挿入を避けるための処理
    ;; ポイント直前のコマンド名の長さ分の文字列を切り出し
    (let ((prev-str (buffer-substring-no-properties
                     (point) (- (point) (length my-command))))
          )
      ;; ポイント直前の文字列がコマンド名と同じでなければ
      (when (not (string= prev-str my-command))
        ;; コマンド名を挿入する
        (insert my-command))))))

;;
;; 実行画面(*shell*バッファー)の末尾にコマンド名を挿入
;;
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
   ))


;;
;; 指定したバッファに警告があるか調べる
;;
(defun compilation-buffer-has-warning-p (buf)
  (save-current-buffer
    (set-buffer buf)
    (save-excursion
      (goto-char (point-min))
      (if (or (search-forward "warning:" nil t)
              (search-forward "警告:" nil t)) t nil))))

;;
;; 指定したバッファのあるウィンドウの表示をflycheck-errorsに切り替える
;;
(defun switch-window-buffer-to-flycheck-errors-from (buf)
  (cond ((get-buffer "*Flycheck errors*")
         (set-window-buffer
          (get-buffer-window buf)
          (get-buffer "*Flycheck errors*"))
         )
        (t
         (delete-windows-on buf)))
  t)

;;
(defun flycheck-errors-has-list-p ()
  (cond ((get-buffer "*Flycheck errors*")
         (save-current-buffer
           (set-buffer (get-buffer "*Flycheck errors*"))
           (save-excursion
             (if (eq (point-min) (point-max))
                 nil t))))
        (t
         nil)))


;; compilationバッファに
;; エラー・警告があればflycheck-errorsに切り替え
;; エラー・警告がなければshellに切り替える
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


;; compile終了時に実行する関数リストに追加
(unless compilation-finish-functions
  (setq compilation-finish-functions nil))
(add-to-list 'compilation-finish-functions
             'switch-compilation-buffer)

;;
;; 指定した番号(通常は2桁)を持つ演習用プログラム「progXX.c」を開く
;;
(defun find-practice-file (number)
  "指定した番号(通常は2桁)を持つ演習用プログラム「progXX.c」を開く"
  (interactive "nProgram Number: ")
  (let ((file-name (format "~/prog%02d.c" number))
        )
    (when (eq buffer-file-name nil)
      (other-window 1))
    (find-file file-name)
    ))

;;
;; 現在のファイル全体をインデントする
;;
(defun indent-whole-file ()
  (interactive)
  (when (not (eq buffer-file-name nil))
    ;; ファイルバッファーにいる
    (let ((file-ext (file-name-extension buffer-file-name t))
          )
      (when (string= file-ext ".c")
        ;; Cのソースファイルのバッファーにいる
        (indent-region (point-min) (point-max))
        ))))

;; emacs-lisp-mode用の設定
(add-hook 'emacs-lisp-mode-hook 'setup-development-packages)

(defun setup-development-packages ()
  "Set up development packages"
  (interactive)

  (when (require 'smartparens nil t)
    (smartparens-strict-mode t)
    (turn-off-show-smartparens-mode))

  (when (require 'swiper nil t)
    (global-set-key (kbd "C-s") 'swiper))

  (when (require 'counsel nil t)
    (global-set-key (kbd  "M-x") 'counsel-M-x))

  (when (require 'highlight-defined nil t)
    (highlight-defined-mode t))

  (when (require 'paren nil t)
    (if (fboundp 'show-paren-local-mode)
        (show-paren-local-mode t)
      (setq-local show-paren-mode t))

    (setq-local show-paren-style 'expression)
    (setq-local show-paren-when-point-inside-paren t)
    ;; (setq-local show-paren-when-point-in-periphery t)

    (face-remap-add-relative 'show-paren-match
                             'my-paren-match-remap-style
                             )
    )

  (when (require 'bm nil t)
    (global-set-key (kbd "<f2>") 'bm-next)
    (global-set-key (kbd "S-<f2>") 'bm-toggle)
    (set-face-attribute 'bm-face nil
                        :foreground "black"
                        :background "#c0dcc0")
    )

  ;; 全角スペース"　"などの外見を変更
  (when (fboundp 'whitespace-mode)
    (whitespace-mode t))

  (setq indent-tabs-mode nil)
  )
