
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

上記の例を説明するにあたって、以下のポイントを見ていきます:
1. デフォルトの`noop logger`を置き換える新しい`Logger`が作成され、デバッグ情報とエラーの出力を`stdout`に設定します。
2. `Env`は`newEnv`を使って作成されます。これは新しい`HTTP Manager`を作成し、アプリケーションのデフォルトの`Credentials`を取得します。
3. `envLogger`と`envScopes`レンズは、それぞれ新しく作成された`Logger`とOAuth2 スコープを設定するのに使われます。
   `Env`に対してスコープを明示的に指定することで、`runGoogle`で実行される`remote operation`とCredentialsのスコープの相違がコンパイルエラーとして検出されるようになります。
  詳しくは[Authorization](#Authorization)を参照してください。
4. オブジェクトに対するストリーミング`body`は`FilePath`から生成され、MIME typeはその拡張子から決定されます。
   MIME typeはCloud Strageではオブジェクトの`Content-Type`として扱われ、`bodyContentType`レンズを以下のように使用することで上書きできます:
```haskell
    > import Network.HTTP.Media ((//))
    >
    > body <- sourceBody f <&> bodyContentType .~ "application" // "json"
```
5. 最終的に、`runResourceT . runGoogle`を使って`Google`モナドを走らせます。
   これによって`ObjectsInsert`型をHTTPリクエストにシリアライズし、ストリーミングを`body`に設定します。
   戻り値の`Object`メタデータはHTTPレスポンスからパースされます。

他の例は[brendanhay/Gogol](https://github.com/brendanhay/gogol/tree/develop/examples)で見ることができます。


