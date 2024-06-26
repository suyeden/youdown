

# youdown

[![GitHub license](<https://img.shields.io/github/license/suyeden/youdown?color=blue>)](<https://github.com/suyeden/youdown/blob/master/LICENSE>)  


## 概要

Youtube video Downloader  

Youtube の動画をダウンロードする Emacs Lisp スクリプトです。（Windows 向け）  
利用には yt-dlp と FFmpeg のインストールが必要です。  


## 動作環境

-   Windows 10 Home
-   GNU Emacs 27.1
-   master-lib 1.0.0 （<https://github.com/suyeden/master-lib>）
-   yt-dlp
-   FFmpeg


## 導入方法

1.  GNU Emacs をダウンロードして適当な場所に展開した上で、Emacs へのパスを通してください。
2.  yt-dlp をダウンロードして適当な場所に展開した上で、yt-dlp へのパスを通してください。
3.  FFmpeg をダウンロードして適当な場所に展開した上で、FFmpeg へのパスを通してください。
4.  Releases から `youdown.zip` をダウンロードし、展開してから中身を適当な場所に配置してください。  
    （このとき、中身の `youdown.bat` ファイルと `youdown` フォルダは同じ場所に置くようにしてください。）


## 使用方法

`youdown.bat` を実行することで、youdown が起動します。  
「Windows によって PC が保護されました」というメッセージが出てきた場合は、「詳細情報」を押してから「実行」をクリックすると以降使えるようになります。  

以下、起動後の各モードの説明です  


### ダウンロードの開始

何も入力せずに Enter を押すとこのモードが選択されます。  
URL が羅列された `ydlist.txt` の内容を読み取り、上から順にダウンロードを始めます。  
`ydlist.txt` は、「ダウンロードリストの作成」を選択するか自力で作るかして用意してください。  


### ダウンロードリストの作成

ダウンロード操作を行うために必要な `ydlist.txt` の作成を行います。  
中身には URL を上から順に羅列してください。  


### 対話実行モード

`ydlist.txt` を必要とせずに、対話的に URL を取得することでダウンロードを実行します。  


### 単純実行モード

対話的に URL を取得していく点は対話実行モードと同じですが、ログファイルの記録などの複雑な操作は一切行わず、単純に URL を yt-dlp に引数として与えていくだけのモードです。  
通常のダウンロードや対話実行モードでは、プレイリストの URL を与えるとエラーとなるため、プレイリストのダウンロードができるのは現状このモードだけとなっています。  

また以下はその他の質問事項の解説です  


### クッキーの利用

年齢制限のある動画などは、Youtube にログインしなければダウンロードすることができません。  
本スクリプトにログイン機能はありませんが、代わりにログイン後に取得したクッキーファイルを用いることで、これらの動画のダウンロードを実現しています。  
クッキーファイルの取得方法はネットを参考にしてください。Firefox などに便利な拡張機能があったはずです。  


### 再接続までの秒数

ダウンロードに時間がかかりすぎる場合は接続状態が悪いことが予想され、再接続することで状況の改善を期待できるため、接続状態の良し悪しの判断を秒数によって行い、指定した秒数の経過を検知することで再接続を試みます。  


## 注意事項

動画のダウンロードにあたっては、著作権法に違反しない範囲で、また個人の責任のもとで行ってください。  

