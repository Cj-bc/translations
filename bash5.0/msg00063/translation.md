= [Qiita] [日本語訳] bash5.0がリリースされました

bash5がついにリリースされました。
といっても知人から言われて気づいたのですが...
そこで、メンテナのChet Ramey氏の通知を和訳してみます。

原文: http://lists.gnu.org/archive/html/bug-bash/2019-01/msg00063.html
ここにたどり着く前のリンク: https://unix.stackexchange.com/questions/478590/whats-going-to-be-new-in-bash-5

---

# 導入

始めてのbash5.0公式リリースが以下のURLから取得可能となりました

ftp://ftp.cwru.edu/pub/bash/bash-5.0.tar.gz
ftp://ftp.gnu.org/pub/gnu/bash/bash-5.0.tar.gz

上記と、bashのgitレポジトリ(http://git.savannah.gnu.org/cgit/bash.git/log/)のmasterブランチ、そしていつも通りGNUのミラーサイトです。

BashはGNUプロジェクトのBourne Again SHellであり、完全なPOSIXでありながらインタラクティブなコマンドライン編集、サポートされているアーキテクチャでのjobコントロール機能、履歴補完やブレース展開、そのほか様々なcshライクな機能を持っています。このタイプのシェル[^?1]にとっては新しいbashの機能について詳しくは、`doc/bashref.texi`を参照してください。 他にも、膨大な量のUnixスタイルのmanページがあります。manページは、shellの機能についての一番信用できる説明です。 

このtarファイル[^?2]は整形されたドキュメントを含んでいます(pdf, postscript,
dvi, info, html, そしてnroffバージョンのマニュアルページ)

このバージョンのバグは`bashbug`を用いて報告してください。このコマンドは、bashと同時にビルド・インストールされます。

# インストール

まず README を読んでください。
インストール方法は INSTALL ファイルに記載されています。

# 新機能

これはbashの5番目のメジャーリリースです。

bash-5.0の新たな機能についての完全な説明は NEWS ファイルを呼んでください。実質的価値のある部分は以下にも含まれています。

このリリースは、bash-4.4にあったいくつかの未解決だったバグを修正し、いくつかの新機能を提供します。修復されたもっとも重大なバグは

---

[^?1]: 「このタイプ」とはどのタイプのこと？POSIX準拠shellということで良い？？
[^?2]: どれ？？
[2019-01-09 18:43]

