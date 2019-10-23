
# 概要
- Module      : Network.Google
- Copyright   : (c) 2015-2016 Brendan Hay
- License     : Mozilla Public License, v. 2.0.
- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
- Stability   : provisional
- Portability : non-portable (GHC extensions)

このモジュールは、`Google`モナドとremote Google Service APIsに使える一般的な操作を提供します。
使いたいサービスに必要な`gogol-*`ライブラリと併用してimportしてください。

# $usage

`gogol-*`ライブラリ群で提供されるリクエストとレスポンスの型は、
リクエストの目的に合わせて`send`, `upload`, `download`と組み合わせて使えます。
名前通り、`send`はリクエストを送信するのに最もよく使うであろう関数で、`upload`と`download`はリクエストとレスポンスのストリーミングを扱う際に便利です。

始めるには、Google Service credentialsを指定し、`Env`環境を作る必要があります。
`Env`は`runGoogle`がアクションを実行するときに使われる設定を保持しています。
<?>
Googleの`Credentials`はいくつかの方法で取得できます。
Google App EngineとGoogle Compute Engineには、Gogolに[Application Default Credentials](https://developers.google.com/identity/protocols/application-default-credentials)を使わせることができます。
もしくは、明示的に自分のcredentialsを使うこともできます。
対応しているcredentialsのメカニズムについて詳しくは[#credentials Credentials]のセクションを参照してください。
</?>

下記の例は、[gogol-storage](http://hackage.haskell.org/package/gogol-storage)の`ObjectsInsert`を使い、[Google Cloud Storage](https://cloud.google.com/storage/)にファイルをアップロードするものです:
```haskell
> import Control.Lens           ((&), (.~), (<&>), (?~))
> import Data.Text              (Text)
> import Network.Google
> import Network.Google.Storage
> import System.IO              (stdout)
>
> import qualified Data.Text as Text
>
> example :: IO Object
> example = do
>     lgr  <- newLogger Debug stdout                                               -- (1)
>     env  <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ storageReadWriteScope) -- (2) (3)
>     body <- sourceBody "/path/to/image.jpg"                                      -- (4)
>
>     let key = "image.jpg"
>         bkt = "my-storage-bucket"
>
>     runResourceT . runGoogle env $                                               -- (5)
>         upload (objectsInsert bkt object' & oiName ?~ key) body
```

