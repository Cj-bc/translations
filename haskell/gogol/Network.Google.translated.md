
# 概要
- Module      : Network.Google
- Copyright   : (c) 2015-2016 Brendan Hay
- License     : Mozilla Public License, v. 2.0.
- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
- Stability   : provisional
- Portability : non-portable (GHC extensions)

このモジュールは、`Google`モナドとremote Google Service APIsに使える一般的な操作を提供します。
使いたいサービスに必要な`gogol-*`ライブラリと併用してimportしてください。

# usage

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


# Authorization

<?>特定の`runGoogle`の文脈内のリクエストは、与えられた認証情報の特定のOAuth2スコープを</?>
例えば、Google Storageの`ObjectsInsert`は以下のスコープを持っています:
```haskell
> type Scopes ObjectsInsert =
>      '["https://www.googleapis.com/auth/cloud-platform",
>        "https://www.googleapis.com/auth/devstorage.full_control",
>        "https://www.googleapis.com/auth/devstorage.read_write"]
```

一つの`runGoogle`の中にある複数の異なるリクエストは、それぞれのリクエストに必要なスコープの最小の集合である必要があります。
この認証情報は`Google`と`MonadGoogle`の型パラメーターの`s`として、型レベルの集合として表されます。
送信されたリクエストのスコープと`Env`の認証スコープの相違はコンパイルエラーになります。
`allow`もしくは`envScopes`レンズを使うことで`Env`のスコープを指定することができます。
様々な`gogol-*`ライブラリはそれぞれ個々のスコープを`Network.Google.*`からエキスポートしており、`(!)`コンビネーターを使ってより大きな集合にすることができます。
例えば:
```haskell
> import Control.Lens ((<&>), (.~))
> import Network.Google
> import Network.Google.Monitoring
>
> main :: IO ()
> main = do
>     env <- newEnv <&> envScopes .~ (monitoringReadScope ! monitoringWriteScope ! computeReadOnlyScope)
>     ...
>>> :type env
Env '["https://www.googleapis.com/auth/monitoring.read", "https://www.googleapis.com/auth/monitoring.write", "https://www.googleapis.com/auth/compute.readonly"]
```

# configuration
各サービスは、ホスト・ポート・パスのプレフィックス・タイムアウト等、他のサービスとは切り離して設定可能な固有の設定を持っています。
サーバーのエンドポイントをモックするときや、特定のリクエストに対するHTTPレスポンスのタイムアウトを調整する時に変更することが望ましいです。
例えば、Google Computeへの全ての呼び出しを実際のエンドポイントの代わりに'https://localhost'に送りたい場合、`Control.Monad.Reader.local`を`override`とともに使うことができます:
```haskell
> import Control.Lens ((&), (.~))
> import Control.Monad.Reader (local)
> import Network.Google
> import Network.Google.Compute
>
> local (override (computeService & serviceHost .~ "localhost")) $ do
>    _ <- send $ instancesGet "project" "zone" "instance-id"
>    ...
```

# Credentials
デフォルトでは`newEnv`はGoogleの公式ライブラリの仕様と同様に、認証情報を探すために`getApplicationDefault`を使用します。
もし`newEnvWith`越しに認証情報を手動で設定したい場合、以下の対応した認証情報メカニズムを使用することができます。
* "Network.Google.Auth.InstalledApplication" - デバイスにインストールされているアプリケーション
* "Network.Google.Auth.ServiceAccount" - 改造された環境にデプロイされたアプリケーション
* "Network.Google.Auth.ApplicationDefault" - App Engine (GAE)もしくはCompute Engine (GCE)上にデプロイされたアプリケーション
詳しくは`Network.Google.Auth`を参照してください。


# async
リクエストは非同期的に送ることができますが、資源の保証のためにクロージャが[lifted-async](http://hackage.haskell.org/package/lifted-async)の使用を要求します。
