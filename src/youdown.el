;;; youdown.el --- script for downloading Youtube videos -*- Emacs-Lisp -*-

;; Copyright (C) 2024 suyeden

;; Author: suyeden
;; Version: 1.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "27.1") (master-lib "1.0.0") (yt-dlp) (ffmpeg))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A wrapper for yt-dlp to download Youtube videos from command line easily.

;;; Code:

(defvar ext-list
  '("mp4" "flv" "webm" "mkv" "wmv" "mpeg" "mpg" "m4a" "m4v" "avi" "mov" "m2ts" "ts" "qt" "asf" "ogm")
  "動画形式の拡張子リスト")
(defvar youdown-dl-stop-flag nil
  "動画ダウンロードの中断を検知")
(defvar ydl-error-counter nil
  "接続試行回数")
(defvar ydl-lib-path (expand-file-name (format "%s/../lib" load-file-name))
  "ライブラリは lib ディレクトリに格納する")
(defvar filename-size nil
  "ファイル名文字数の上限")

(defun main ()
  "Youtube の動画をダウンロードするスクリプト"
  (let ((youdown-mode nil))
    ;; ライブラリのロード
    (load (expand-file-name "master-lib.el" (format "%s/../lib" load-file-name)) nil t)
    ;; 初期化
    (my-init)
    ;; モード判別
    (while (not (or (string= "1" youdown-mode) (string= "2" youdown-mode) (string= "3" youdown-mode) (string= "4" youdown-mode) (string= "q" youdown-mode)))
      (setq youdown-mode (my-read-str "\n 1) ダウンロードの開始 （デフォルト）\n 2) ダウンロードリストの作成\n 3) 対話実行モード\n 4) 単純実行モード\n q) Quit\n 利用するモードを番号で指定 : "))
      (when (string= "" youdown-mode)
        (setq youdown-mode "1")))
    (princ "\n")
    ;; ファイル名文字数上限取得
    (switch-to-buffer "*tmp*")
    (insert default-directory)
    (let ((filename-max-size 0))
      (while (not (= (point) (point-min)))
    	  (forward-char -1)
    	  (setq filename-max-size (1+ filename-max-size)))
      (setq filename-size (- 244 (* 2 filename-max-size))))
    (kill-buffer (current-buffer))
    ;; モード実行
    (if (string= "1" youdown-mode)
        (youdown-dl-with-ydlist)
      (if (string= "2" youdown-mode)
          (youdown-make-list)
        (if (string= "3" youdown-mode)
            (youdown-dl-interactive)
          (if (string= "4" youdown-mode)
              (youdown-dl-primitive)
            (if (string= "q" youdown-mode)
                (princ "\n Quit.\n\n")
              nil)))))))

(defun youdown-make-list ()
  "ダウンロードリストを作成する"
  (my-princ " \"C-x C-s\" で保存、\"C-x C-c\" でエディタを終了してください\n なお、 \"C\" とは Ctrl キーのことです\n")
  (shell-command-to-string "emacs ydlist.txt")
  (if (file-exists-p "ydlist.txt")
      (if (my-read-str "\n ダウンロードリストが作成されました！ ダウンロードを開始しますか？\n （何か入力して Enter を押すとダウンロードが始まります）\n : ")
          (progn
            (princ "\n")
            (youdown-dl-with-ydlist))
        (princ "\n\n Bye!\n\n"))
    (princ "\n\n Bye!\n\n")))

(defun youdown-dl-with-ydlist ()
  "ダウンロードリストに基づいてダウンロードする"
  (let ((url-dl nil)
        (filename-tmp nil)
        (filename-real nil)
        (list-file "ydlist.txt")
        (log-file "ydl-log.txt")
        (error-log-txt "ydl-error.txt")
        (error-log-html "ydl-error.html")
        (output-dir ".youdown_tmp")
        (time-limit 0)
        (time-limit-default 150)
        (cookie-path nil))
    (if (file-exists-p list-file)
        ;; ダウンロードリストが存在するとき
        (if (string= "t" (check-output-dir output-dir))
            ;; 一時ディレクトリ関係がスムーズにいったとき
            (progn
              ;; ログファイル初期化
              (output-log-init log-file)
              ;; クッキーの有無確認
              (catch 'ydwy-read-cookie-path-end
                (while (equal nil cookie-path)
                  (setq cookie-path (my-read-str " クッキーを利用する場合はパスを入力してください\n （無ければそのまま Enter を押下、フォルダー名が入力された場合はそのフォルダー下の \"cookies.txt\" が指定されます）\n : "))
                  (when (string= "" cookie-path)
                    (setq cookie-path nil)
                    (throw 'ydwy-read-cookie-path-end t))
                  (if (file-exists-p (expand-file-name cookie-path))
                      (if (file-directory-p (expand-file-name cookie-path))
                          (setq cookie-path (expand-file-name "cookies.txt" (expand-file-name cookie-path)))
                        (setq cookie-path (expand-file-name cookie-path)))
                    (setq cookie-path nil))))
              (princ "\n")
              ;; 再接続のための秒数取得
              (setq time-limit (string-to-number (my-read-str (format " 再接続までの秒数を入力 （デフォルトは %s 秒）\n ダウンロード完了までに時間がかかりすぎる場合は、この秒数を基に再接続を試みます\n : " time-limit-default))))
              (if (<= time-limit 0)
                  (setq time-limit time-limit-default))
              (princ "\n")
              ;; ダウンロードループ
              (catch 'youdown-with-ydlist-stop
                (while (string= "t" (judge-end list-file))
                  (my-princ (format "\n 残りURL : %s\n\n" (my-count-line list-file)))
                  (setq url-dl (read-file-head list-file))
                  (setq filename-tmp (return-filename-scheduled url-dl cookie-path))
                  (my-princ (format " \"%s\"\n をダウンロード中 ...\n\n" filename-tmp))
                  (cd output-dir)
                  (youdown-dl url-dl time-limit cookie-path 90)
                  (setq ydl-error-counter 0)
                  (cd "../")
                  (when (equal t youdown-dl-stop-flag)
                    (throw 'youdown-with-ydlist-stop t))
                  (setq filename-real (move-file-dl (judge-file-dl output-dir) output-dir))
                  (write-log filename-real url-dl filename-tmp log-file error-log-txt error-log-html)
                  (when (string= url-dl (read-file-head list-file))
                    (delete-file-head list-file))))
              ;; ダウンロード終了後処理
              (when (file-exists-p output-dir)
                (sit-for 1)
                (delete-directory output-dir t))
              (my-princ "\n ダウンロード終了！\n\n"))
          ;; 既に存在する一時ディレクトリを使用しないとき
          (princ "\n Bye!\n\n"))
      ;; ダウンロードリストが存在しないとき
      (if (not (string= "" (my-read-str " ダウンロードリストが存在しません\n ダウンロードリストを作成しますか？（何か入力して Enter を押すとリストの作成に移ります）\n : ")))
          (progn
            (princ "\n")
            (youdown-make-list))
        (princ "\n Bye!\n\n")))))

(defun youdown-dl-interactive ()
  "インタラクティブに URL を入力してダウンロードする"
  (let ((url nil)
        (url-list nil)
        (cookie-path nil)
        (time-limit 0)
        (time-limit-default 120)
        (filename-tmp nil)
        (filename-real nil)
        (list-file "ydlist.txt")
        (log-file "ydl-log.txt")
        (error-log-txt "ydl-error.txt")
        (error-log-html "ydl-error.html")
        (output-dir ".youdown_tmp"))
    (if (string= "t" (check-output-dir output-dir))
        ;; 一時ディレクトリ関係がスムーズにいったとき
        (progn
          ;; URL 入力
          (while (not (string= "" (setq url (my-read-str " URL ? (何も入力せずに Enter 押下で終了) : "))))
            (setq url-list (cons url url-list)))
          (setq url-list (reverse url-list))
          (princ "\n")
          ;; ログファイル初期化
          (output-log-init log-file)
          ;; クッキーの有無確認
          (catch 'ydi-read-cookie-path-end
            (while (equal nil cookie-path)
              (setq cookie-path (my-read-str " クッキーを利用する場合はパスを入力してください\n （無ければそのまま Enter を押下、フォルダー名が入力された場合はそのフォルダー下の \"cookies.txt\" が指定されます）\n : "))
              (when (string= "" cookie-path)
                (setq cookie-path nil)
                (throw 'ydi-read-cookie-path-end t))
              (when (file-exists-p (expand-file-name cookie-path))
                (if (file-directory-p (expand-file-name cookie-path))
                    (setq cookie-path (expand-file-name "cookies.txt" (expand-file-name cookie-path)))
                  (setq cookie-path (expand-file-name cookie-path))))
              (setq cookie-path nil)))
          (princ "\n")
          ;; 再接続のための秒数取得
          (setq time-limit (string-to-number (my-read-str (format " 再接続までの秒数を入力 （デフォルトは %s 秒）\n ダウンロード完了までに時間がかかりすぎる場合は、この秒数を基に再接続を試みます\n : " time-limit-default))))
          (if (<= time-limit 0)
              (setq time-limit time-limit-default))
          (princ "\n")
          ;; ダウンロードループ
          (catch 'youdown-interactive-stop
            (while url-list
              (my-princ (format "\n 残りURL : %s\n\n" (length url-list)))
              (setq filename-tmp (return-filename-scheduled (car url-list) cookie-path))
              (my-princ (format " \"%s\"\n をダウンロード中 ...\n\n" filename-tmp))
              (cd output-dir)
              (youdown-dl (car url-list) time-limit cookie-path 60)
              (setq ydl-error-counter 0)
              (cd "../")
              (when (equal t youdown-dl-stop-flag)
                (throw 'youdown-interactive-stop t))
              (setq filename-real (move-file-dl (judge-file-dl output-dir) output-dir))
              (write-log filename-real (car url-list) filename-tmp log-file error-log-txt error-log-html)
              (setq url-list (cdr url-list))))
          ;; ダウンロード終了後処理
          (when (file-exists-p output-dir)
            (sit-for 1)
            (delete-directory output-dir t))
          (my-princ "\n ダウンロード終了！\n\n"))
      ;; 既に存在する一時ディレクトリを使用しないとき
      (princ "\n Bye!\n\n"))))

(defun youdown-dl-primitive ()
  "ダウンロード単純実行モード"
  (let ((url nil)
        (url-list nil)
        (cookie-path nil)
        (time-limit 0)
        (time-limit-default 120)
        (filename-tmp nil))
    ;; URL 入力
    (while (not (string= "" (setq url (my-read-str " URL ? (何も入力せずに Enter 押下で終了) : "))))
      (setq url-list (cons url url-list)))
    (setq url-list (reverse url-list))
    (princ "\n")
    ;; クッキーの有無確認
    (catch 'ydp-read-cookie-path-end
      (while (equal nil cookie-path)
        (setq cookie-path (my-read-str " クッキーを利用する場合はパスを入力してください\n （無ければそのまま Enter を押下、フォルダー名が入力された場合はそのフォルダー下の \"cookies.txt\" が指定されます）\n : "))
        (when (string= "" cookie-path)
          (setq cookie-path nil)
          (throw 'ydp-read-cookie-path-end t))
        (when (file-exists-p (expand-file-name cookie-path))
          (if (file-directory-p (expand-file-name cookie-path))
              (setq cookie-path (expand-file-name "cookies.txt" (expand-file-name cookie-path)))
            (setq cookie-path (expand-file-name cookie-path))))
        (setq cookie-path nil)))
    (princ "\n")
    ;; 再接続のための秒数取得
    (setq time-limit (string-to-number (my-read-str (format " 再接続までの秒数を入力 （デフォルトは %s 秒）\n ダウンロード完了までに時間がかかりすぎる場合は、この秒数を基に再接続を試みます\n : " time-limit-default))))
    (if (<= time-limit 0)
        (setq time-limit time-limit-default))
    (princ "\n")
    ;; ダウンロードループ
    (catch 'youdown-primitive-stop
      (while url-list
        (my-princ (format "\n 残りURL : %s\n\n" (length url-list)))
        (setq filename-tmp (return-filename-scheduled (car url-list) cookie-path))
        (my-princ (format " \"%s\"\n をダウンロード中 ...\n\n" filename-tmp))
        (youdown-dl-simple (car url-list) time-limit cookie-path 60)
        (setq ydl-error-counter 0)
        (when (equal t youdown-dl-stop-flag)
          (throw 'youdown-primitive-stop))
        (setq url-list (cdr url-list))))
    (my-princ "\n ダウンロード終了！\n\n")))

(defun youdown-dl (url time-limit cookie-path add-time)
  "ダウンロード操作本体"
  (let ((counter 0)
        (error-max 5))
    (catch 'youdown-dl-stop
      (catch 'youdown-dl-end
        (if (equal nil cookie-path)
            ;; (my-start-process-shell-command "ydl" "ydl" (format "yt-dlp --external-downloader aria2c --external-downloader-args \"aria2c:-c -x 16 -s 16 -j 16 -k 1M -m 0\" -f bestvideo+bestaudio/best -o \"%s\" \"%s\"" "%(title)s.%(ext)s" url))
            (my-start-process-shell-command "ydl" "ydl" (format "yt-dlp -N 5 --trim-filenames %s -f bestvideo+bestaudio/best -o \"%s\" \"%s\"" filename-size "%(title)s.%(ext)s" url))
          ;; (my-start-process-shell-command "ydl" "ydl" (format "yt-dlp --external-downloader aria2c --external-downloader-args \"aria2c:-c -x 16 -s 16 -j 16 -k 1M -m 0\" -f bestvideo+bestaudio/best --cookies \"%s\" -o \"%s\" \"%s\"" cookie-path "%(title)s.%(ext)s" url))
          (my-start-process-shell-command "ydl" "ydl" (format "yt-dlp -N 5 --trim-filenames %s -f bestvideo+bestaudio/best --cookies \"%s\" -o \"%s\" \"%s\"" filename-size cookie-path "%(title)s.%(ext)s" url)))
        (while (<= counter time-limit)
          (sit-for 1)
          ;; ダウンロードの中断を検知
          (when (or (file-exists-p "../ydl-stop") (file-exists-p "../ydl-stop.txt"))
            (delete-process "ydl")
            (setq youdown-dl-stop-flag t)
            (throw 'youdown-dl-stop t))
          ;; ダウンロードの終了を検知
          (if (get-process "ydl")
              (setq counter (1+ counter))
            (throw 'youdown-dl-end t)))
        ;; タイムリミットを超えたとき
        ;; ダウンロード後の変換作業中の場合はそのまま続ける
        ;; 終了し次第 end フラグに飛ぶ
        (if (string= "t" (find-tmp-file))
            (progn
              (my-princ " ただいま変換中！ 少しお待ちください ... \n\n")
              (catch 'converting-end
                (while t
                  (sit-for 1)
                  (unless (get-process "ydl")
                    (throw 'converting-end t)))))
          ;; ダウンロードが終了していなければやり直し
          ;; 再接続の度に制限時間を add-time 秒伸ばす
          (delete-process "ydl")
          (my-princ " 再接続します\n\n")
          (youdown-dl url (+ time-limit add-time) cookie-path add-time)))
      ;; ダウンロード終了後、カレントディレクトリに動画が一つだけ存在し、なおかつそれが動画ファイルであった場合はそのまま終了する
      ;; そうでない場合は、再びダウンロードを実行する
      ;; error-max回試行しても終了しなかった場合はそのまま終了する
      (unless (judge-file-dl ".")
        (when (and (< ydl-error-counter error-max) (string= "nil" (format "%s" youdown-dl-stop-flag)))
          (setq ydl-error-counter (1+ ydl-error-counter))
          (my-princ (format " 再試行 （%s/%s） ...\n\n" ydl-error-counter error-max))
          (youdown-dl url time-limit cookie-path add-time))))))

(defun youdown-dl-simple (url time-limit cookie-path add-time)
  "簡易版 youdown-dl 関数"
  (let ((counter 0)
        (error-max 5))
    (catch 'youdown-dl-simple-end
      (if (equal nil cookie-path)
          (my-start-process-shell-command "ydl" "ydl" (format "yt-dlp -N 5 --trim-filenames %s -f bestvideo+bestaudio/best -o \"%s\" \"%s\"" filename-size "%(title)s.%(ext)s" url))
        (my-start-process-shell-command "ydl" "ydl" (format "yt-dlp -N 5 --trim-filenames %s -f bestvideo+bestaudio/best --cookies \"%s\" -o \"%s\" \"%s\"" filename-size cookie-path "%(title)s.%(ext)s" url)))
      (while (<= counter time-limit)
        (sit-for 1)
        ;; ダウンロードの中断を検知
        (when (or (file-exists-p "./ydl-stop") (file-exists-p "./ydl-stop.txt"))
          (delete-process "ydl")
          (setq youdown-dl-stop-flag t)
          (throw 'youdown-dl-simple-end t))
        ;; ダウンロードの終了を検知
        (if (get-process "ydl")
            (setq counter (1+ counter))
          (throw 'youdown-dl-simple-end t)))
      ;; タイムリミットを超えたとき
      ;; ダウンロードが終了するまで繰り返しやり直す
      ;; 再接続の度に制限時間を add-time 秒伸ばす
      (delete-process "ydl")
      (my-princ " 再接続します\n\n")
      (youdown-dl-simple url (+ time-limit add-time) cookie-path add-time))))

(defun find-tmp-file ()
  "カレントディレクトリに '.tmp.拡張子' のファイルが存在すれば t を、そうでなければ nil を返す"
  (let ((ext-list-copy ext-list)
        (all-files-list (directory-files "."))
        (result nil))
    (with-temp-buffer
      (while all-files-list
        (insert (format "%s\n" (car all-files-list)))
        (setq all-files-list (cdr all-files-list)))
      (catch 'find-temp-file-end
        (while ext-list-copy
          (goto-char (point-min))
          (if (re-search-forward (format "\\.temp\\.%s$" (car ext-list-copy)) nil t)
              (progn
                (setq result t)
                (throw 'find-temp-file-end t)))
          (setq result nil)
          (setq ext-list-copy (cdr ext-list-copy)))))
    result))

(defun check-output-dir (output-dir)
  "'output-dir'ディレクトリが存在する場合は引き続き使用するかどうか聞き、引き続き使用する場合は t、そうでない場合は nil を返す
'output-dir'ディレクトリが存在しない場合は作成して t を返す"
  (if (and (file-exists-p output-dir) (file-directory-p output-dir))
      (if (not (string= "" (my-read-str (format " \"%s\" フォルダーが既に存在しています\n 引き続きこのフォルダーを使用する場合は、何かしら入力してから Enter を押してください\n : " output-dir))))
          (progn
            (princ "\n")
            t)
        (princ "\n")
        nil)
    (make-directory output-dir)
    t))

(defun return-filename-scheduled (url cookie-path)
  "ダウンロード予定のファイル名（拡張子抜き）を返す"
  (let (filename)
    (with-temp-buffer
      (insert
       (if (string= "nil" cookie-path)
           ;; (my-shell-command-to-string (format "youtube-dl --get-filename -o \"%s\" %s" "%(title)s.%(ext)s" url))
           (my-shell-command-to-string (format "yt-dlp --get-filename -o \"%s\" %s" "%(title)s.%(ext)s" url))
         ;; (my-shell-command-to-string (format "youtube-dl --get-filename --cookies \"%s\" -o \"%s\" %s" cookie-path "%(title)s.%(ext)s" url))
         (my-shell-command-to-string (format "yt-dlp --get-filename --cookies \"%s\" -o \"%s\" %s" cookie-path "%(title)s.%(ext)s" url))))
      (goto-char (point-min))
      (setq filename (buffer-substring (point) (progn (end-of-line) (point)))))
    (setq filename (get-filename-body filename))
    filename))

(defun get-filename-body (filename)
  "'filename'のうち拡張子を除いたファイル名部分を返す"
  (let (filename-body)
    (with-temp-buffer
      (insert filename)
      (end-of-line)
      (search-backward "." nil t)
      (setq filename-body (buffer-substring (point) (progn (beginning-of-line) (point)))))
    filename-body))

(defun read-file-head (filename)
  "'filename'ファイルの先頭行文字列を返す"
  (let (line-head)
    (find-file filename)
    (goto-char (point-min))
    (setq line-head (buffer-substring (point) (progn (end-of-line) (point))))
    (kill-buffer filename)
    line-head))

(defun delete-file-head (filename)
  "'filename'ファイルの先頭行を削除する"
  (find-file filename)
  (goto-char (point-min))
  (delete-region (point) (progn (forward-line 1) (point)))
  (my-save-buffer)
  (kill-buffer (current-buffer))
  (my-del-extra-file filename))

(defun judge-end (filename)
  "'filename'ファイルが空ファイルなら nil を返し、そうでなければ t を返す"
  (let (end-flag)
    (find-file filename)
    (if (= (point-min) (point-max))
        (setq end-flag nil)
      (setq end-flag t))
    (kill-buffer (current-buffer))
    end-flag))

(defun judge-file-dl (output-dir)
  "'output-dir'ディレクトリ中にファイルが一つだけ存在し、そのファイルの拡張子が動画形式であればそのファイル名を返す
それ以外は nil を返す"
  (let ((all-files nil)
        (result nil))
    (setq all-files (my-exclude-invalid-file (directory-files (format "./%s" output-dir))))
    (if (= 1 (length all-files))
        (if (string= "t" (my-judge-ext (car all-files) ext-list))
            (setq result (car all-files))
          (setq result nil))
      (setq result nil))
    result))

(defun move-file-dl (filename output-dir)
  "'filename'に non-nil を与えると'output-dir'ディレクトリからカレントディレクトリへ'filename'ファイルを移動させ、その移動させたファイル名を返す
'filename'に nil を与えると、'output-dir'ディレクトリ中のファイルを消して、nil を返す"
  (let ((filename-move nil)
        (all-files nil))
    (if (not (string= "nil" filename))
        (progn
          (setq filename-move (my-find-new-filename filename))
          (when (file-exists-p (format "./%s/%s" output-dir filename))
            (rename-file (format "./%s/%s" output-dir filename) (format "./%s" filename-move))))
      (setq all-files (my-exclude-invalid-file (directory-files (format "./%s" output-dir))))
      (while all-files
        (if (file-exists-p (format "./%s/%s" output-dir (car all-files)))
            (delete-file (format "./%s/%s" output-dir (car all-files))))
        (setq all-files (cdr all-files)))
      (setq filename-move nil))
    filename-move))

(defun write-log (title url tmp_title log-filename error-log-txt error-log-html)
  "ログファイル関係の処理を行う
'title'が non-nil のとき、ログファイル'log-filename'に'title', 'url', 日付, 端末出力時のファイル名'tmp_file'を挿入する
'title'が nil のとき、エラーログファイル'error-log-txt'と'error-log-html'に'title'や'url'を挿入する"
  (if (not (string= "nil" title))
      (output-log log-filename title url)
    (my-princ " ダウンロードエラー\n\n")
    (output-error-txt error-log-txt url)
    (output-error-html error-log-html url tmp_title)))

(defun output-log-init (filename)
  "ダウンロード開始時にログファイル'filename'に対して最初に行う操作"
  (find-file filename)
  (goto-char (point-max))
  (if (= (point-min) (point-max))
      (insert (format "%s\n" (my-time)))
    (insert (format "\n\n%s\n" (my-time))))
  (save-buffer)
  (kill-buffer (current-buffer))
  (my-del-extra-file filename))

(defun output-log (filename title url)
  "ログファイル'filename'の最後尾にビデオ名'title'と日付、URL'url'を挿入する"
  (find-file filename)
  (goto-char (point-max))
  (insert (format "%s ( %s , %s )\n" title (my-time) url))
  (my-save-buffer)
  (kill-buffer (current-buffer))
  (my-del-extra-file filename))

(defun output-error-txt (filename url)
  "'filename'ファイルにエラーログ'url'を記録する"
  (find-file filename)
  (goto-char (point-max))
  (insert (format "%s\n" url))
  (my-save-buffer)
  (kill-buffer (current-buffer))
  (my-del-extra-file filename))

(defun output-error-html (filename url title)
  "'filename'ファイルにエラーログ'url'をリンクとして挿入し、動画タイトル'title'をその後ろに挿入する"
  (find-file filename)
  (goto-char (point-max))
  (insert (format "<a href=\"%s\">%s</a>　%s<br>\n" url url title))
  (my-save-buffer)
  (kill-buffer (current-buffer))
  (my-del-extra-file filename))

(defun my-princ (str)
  "標準出力関数"
  (my-print str ydl-lib-path))

(defun my-read-str (str)
  "標準入力関数"
  (my-read-string str ydl-lib-path))

(main)
;;; youdown.el ends here
