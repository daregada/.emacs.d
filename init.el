;;
;; Edu(logos)用のinit.el
;; ~/.emacs.dに置くこと

;;
;; キーカスタマイズ関連
;;
;; Ctrl-Vに「貼り付け」(yank)を割り当て(Windwosユーザー向け対策)
;; Ctrl-Vの元の機能「1画面下を表示」(scroll-up-command)はPagedownで代用可能
(define-key global-map (kbd "C-v") 'yank)

;; Ctrl-Zに「元に戻す」(undo)を割り当て(Windowsユーザー向け対策)
;; Ctrl-Zの元の機能「一時停止」(suspend-emacs)は「M-x suspend-emacs」で代用可能
(define-key global-map (kbd "C-z") 'undo)

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

;; CentOS7のgnome-terminal用(Ctrl-Jが入力できないため)
(define-key global-map (kbd "<f10>") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "<f10>") 'eval-print-last-sexp)


;;
;; 起動時画面関連
;;
;; 先頭行にメニューバーを表示しない(端末版ではマウス操作できないため)
(menu-bar-mode -1)

;; Emacsのスタート画面を消す
(setq inhibit-startup-screen t)

;; 起動時にscratchバッファー先頭のメッセージを出さない
(setq initial-scratch-message nil)

;; ヘッダーラインの見た目を変える
(set-face-foreground 'header-line "black")
(set-face-background 'header-line "orange")
(set-face-attribute 'header-line nil
		    :inherit nil
		    :overline nil
		    :underline t)

;; 軽量バッファーモードの定義
(defun lightweight-buffer-mode ()
  "Lightweight Buffer Mode"
  (interactive)
  (setq mode-name "Lightweight Buffer")
  (setq major-mode 'lightweight-buffer-mode)
  (run-hooks 'lightweight-buffer-mode-hook))

(with-current-buffer (get-buffer "*scratch*")
  ;; 起動時のscratchバッファーをlightweight-buffer-modeに
  (setq initial-major-mode 'lightweight-buffer-mode)
  ;; 先頭行をヘッダーラインに
  (setq header-line-format
	(concat " 注意: ここにC言語のプログラムを書かないでください。"))
  (insert "\nC言語のソースファイル(拡張子.c)を作成/開いて、そこに書いてください。\n\n"))
;;
;; 起動処理が完了後
;;
(defadvice command-line-1 (after my-scratch-is-ready activate)
  "*scratch*に起動後の注意書きを表示"
  (with-current-buffer (get-buffer "*scratch*")
    (when (not (one-window-p))
      (delete-windows-on (get-buffer "*Compile-Log*")))
     
    (goto-char (point-max))
    ;; 注意書きを挿入
    (insert "ファイルを作成/開くには、キー操作(C-x C-f)を使います。\n"
	    "(「C-x」はCtrlキーを押したままXキーを押す操作。その後の「C-f」も同様)\n"
	    "一番下に「Find File:」と出たら、ファイル名を入力しEnterキーを押してください。\n\n"
	    "ヒント: 演習用のファイル(progNN.c)であれば、もっと簡単な方法があります。\n"
	    "「<f7>」(F7キー)を押し、一番下に「Program Number:」と出たら、プログラム番号(1桁か2桁の整数)を入力し、Enterキーを押してください。\n\n"
)
    (insert "起動時の処理が完了しました。\n")
    (set-buffer-modified-p nil)))


;;
;; package.el関連
;;
(require 'package)

;; MELPA-stableのみ追加
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; 初期化
(package-initialize)


;;
;; shell関連
;;
(require 'shell)
;; プロンプトは読み取り専用
(setq comint-prompt-read-only t)
;; プロンプトに返り値($?)を含める
(setenv "PS1" "[\\u@\\h \\W ($(echo $?;))]\\$ ")

;;
;; flycheck関連
;;
;; flychekがインストールされていなければ自動的にインストール
(unless (package-installed-p 'flycheck)
  (with-current-buffer (get-buffer "*scratch*")
    (insert "最初の一回だけ、flycheckパッケージの導入作業を行ないます。\n"
	    "導入作業が終わるまでしばらく時間がかかります。\n\n"))
  (redisplay)
  (unless package--initialized
    (package-initialize))
  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'flycheck)
    (package-install 'flycheck))
  )

(require 'flycheck)

;; flycheck-error-listのID欄の横幅を6->3に
(aset flycheck-error-list-format 3 '("ID" 3 t))

;; チェックに使わないチェッカーの既定値
;; インストールされていないclang, cppcheckと、日本語未対応のc/c++-gcc
(setq-default flycheck-disabled-checkers '(c/c++-clang
					   c/c++-cppcheck
					   c/c++-gcc))

;; flycheckを日本語化されたgccのエラーメッセージに対応させる
;; オリジナル: 'https://futurismo.biz/archives/2992'
;; 最新のfrycheck.elを参照して、エラーパターンを修正
;; note(備考)とfatal error(致命的エラー)の対応を追加
;; 英語のエラーメッセージにも対応できるようにした
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
      (error
       line-start (or "<stdin>" (file-name))
       ":" line (optional ":" column)
       ": " (or "fatal error" "致命的エラー" "error" "エラー") ": " (message) line-end))
     :modes ',modes))

;; C言語用の日本語対応エラーチェッカー(c-gcc-ja)を定義
(flycheck-define-clike-checker c-gcc-ja
			       ("gcc" "-fsyntax-only" "-fshow-column" "-Wall" "-Wextra" "-std=gnu99")
			       c-mode)

;; チェッカーとして登録
(add-to-list 'flycheck-checkers 'c-gcc-ja)

;; C++言語用の日本語対応エラーチェッカーを定義
;; (flycheck-define-clike-checker c++-g++-ja
;;                    ("g++" "-fsyntax-only" "-Wall" "-Wextra" "-std=c++11")
;;                    c++-mode)
;; (add-to-list 'flycheck-checkers 'c++-g++-ja)

;; エラーを表示する関数をエコー領域を使わないものに変更
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;; モード切り替え時と保存時とEnterキー入力時にチェックする 
(setq flycheck-check-syntax-automatically '(mode-enabled save new-line))

;; エラー・警告・備考のフェイス設定
(set-face-foreground 'flycheck-error "white")
(set-face-background 'flycheck-error "red")
(set-face-attribute 'flycheck-error nil :underline nil :weight 'bold)
(set-face-foreground 'flycheck-warning "black")
(set-face-background 'flycheck-warning "orange")
(set-face-attribute 'flycheck-warning nil :underline nil :weight 'bold)
(set-face-foreground 'flycheck-info "lightblue")
					;(set-face-background 'flycheck-info "lightblue")

;; 前回のコンパイルの結果
(setq has-error-or-warnings nil)

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

;; フック
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;;
   ;; show-paren-mode関連
   ;;
   ;; 対応するカッコの強調をc-modeに限定
   (make-variable-buffer-local 'show-paren-mode)
   ;; 対応するカッコの強調を行なう
   (show-paren-mode 1)
   ;; 対応するカッコの片方が画面外なら範囲内すべてを強調
   (setq show-paren-style 'mixed)

   ;;
   ;; c-mode (cc-mode)関連
   ;;
   ;; 字下げはスペースだけで行う(タブ文字を使わない)
   (setq indent-tabs-mode nil)
   ;; Enterキーによる挿入行を字下げする
   (define-key c-mode-base-map "\C-m" 'c-context-line-break)
   ;; BackspaceキーやCtrl-Dキーで複数のスペース・改行を一括削除し、
   (c-toggle-hungry-state 1)
   ;; 「;」や「}」を入力すると自動的に改行する
   ;; (c-toggle-auto-newline 1)
   ;; 「else」と「else if」は、直前の「}」と同じ行にくっつける
   (setq c-cleanup-list
	 '(brace-else-brace
	   brace-elseif-brace
	   defun-close-semi)
	 )
   ;;
   ;; compile関連
   ;;
   ;; 実行形式への変換に使うコマンドの確認(最下行に表示してEnterキー)をしない
   (setq compilation-read-command nil)
   ;; 実行形式への変換前にソースファイルを自動保存する
   (setq compilation-ask-about-save nil)
   ;; 実行形式への変換後、最初のエラーにポイント(カーソル)を移動させる
   (setq compilation-auto-jump-to-first-error t)
   ;; 実行形式への変換後、エラー(と思われる)範囲をハイライト表示する秒数 
   (setq next-error-highlight 5)

   ;;
   ;; flycheck関連
   ;;
   ;; flycheckによる文法チェックを行なう
   (flycheck-mode 1)
   ;; エラー・警告を別バッファーに一覧表示する
   (flycheck-list-errors)
   
   ;;
   ;; バックアップファイルを作らない
   ;;
   (make-local-variable 'make-backup-files)
   (setq make-backup-files nil)
   ))


;;
;; autoinsert関連
;;
(require 'autoinsert)
;; ファイルを新規作成する際にテンプレートを利用する
(add-hook 'find-file-not-found-hooks 'auto-insert)

;; テンプレートを使うかどうかの質問をしない(常に使う)
(setq auto-insert-query nil)

;; テンプレートのリストをクリア
(setq auto-insert-alist nil)

;; C言語用テンプレートを設定
(define-auto-insert
  '("\\.c$" . "C template")
  [("C template: "
    (c-mode)			; テンプレート挿入時はc-modeになっていないため必要
    "#include <stdio.h>" \n \n	; 先頭行と次行(空行)を挿入
    "int main(void)" \n		; mainの引数はvoidにする
    "{" > \n		  	; {の位置を行頭にするには後ろに>を入れてインデント
    > _ \n			; インデント後にポインターを置く
    "return 0;" \n		; 前行と同じインデントでreturn文を置く
    "}" > \n )])		; {と位置を合わせるには後ろに>を入れてインデント

;; .bash_profile用テンプレートを設定
(define-auto-insert
  '("/\\.bash_profile$" . "Bash profile template")
  [("Bash profile template: "
    "export LANG=ja_JP.UTF-8\n"
    "source ~/.bashrc\n"
    )])

;; 
;; 自作コマンドと関数
;; 
(require 'cl-lib)

;; 直前にsave-and-compileで実行形式に変換されたプログラムのコマンド名(先頭に"./"付き)
(defvar my-command "")

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
	 ;; テンプレートを読んだ直後は変更フラグなしのため、問答無用で保存
	 (save-buffer)

	 ;; flycheckによるエラーチェックの結果を更新
	 (when (flycheck-mode)
	   (flycheck-buffer)
	   (sit-for 0.5)
	   )

	 (let
	     ((file-base (shell-quote-argument (file-name-base buffer-file-name)))
	      (file-name (shell-quote-argument (file-name-nondirectory buffer-file-name))))
	   ;; 後で実行するときのコマンド名
	   (setq my-command (format "./%s" file-base))
	   ;; コンパイルコマンドにファイル名などを埋め込む
	   (set (make-local-variable 'compile-command)
		(format "gcc -Wall -Wextra -std=gnu99 -lm -o%s %s"
			file-base
			file-name
			))
	   ;; コンパイルを実行。画面が自動分割されて変換結果が表示される
	   ;; flycheck-errors-listに切り替わる
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
    ;; shellバッファーから別のウィンドウに移動するときはflycheck-errorsに切り替える
    (when (string= (buffer-name) "*shell*")
      (switch-buffer-to-flycheck-errors "*shell*")))
  ;; ウィンドウが分割されていれば、別ウィンドウを順に選択する
  (if (and (not (flycheck-errors-has-list-p))
	   (string= (buffer-name) "*Flycheck errors*")
	   (eq has-error-or-warnings t))
      (progn
	(set-window-buffer
	 (get-buffer-window (current-buffer))
	 (get-buffer "*compilation*"))
      )
    (when (string= (buffer-name) "*compilation*")
      (switch-buffer-to-flycheck-errors "*compilation*"))
    (other-window 1)))


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
    (process-kill-without-query (get-process "shell"))
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
	(process-kill-without-query (get-process "shell"))
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
;; コンパイル結果のバッファをflycheck-errorsに切り替える
;;
(defun switch-buffer-to-flycheck-errors (buf)
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
	     (switch-buffer-to-flycheck-errors "*compilation*")))
	(t
	 (setq has-error-or-warnings nil)
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
    (find-file file-name)
    ))

;; emacs-lisp-mode用の設定
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (when (require 'paredit nil t)
	       (enable-paredit-mode))
	     (show-paren-mode)
	     ))
