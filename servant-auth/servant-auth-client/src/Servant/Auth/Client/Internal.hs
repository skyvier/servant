{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ == 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Servant.Auth.Client.Internal where

import qualified Data.ByteString    as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Proxy         (Proxy (..))
import           Data.String        (IsString)
import           Data.Text.Encoding (decodeUtf8)
import           Data.CaseInsensitive
import           GHC.Generics       (Generic)
import           Servant.API        ((:>))
import           Servant.Auth

import           Servant.API (ToHttpApiData(..))
import           Servant.Client.Core
import           Servant.Client.Core.Auth

import GHC.TypeLits
import Data.Kind

-- | A simple bearer token.
newtype Token = Token { getToken :: BS.ByteString }
  deriving (Eq, Show, Read, Generic, IsString)

instance ToHttpApiData Token where
  toUrlPiece (Token bs) = decodeUtf8 bs
  toHeader (Token bs) = bs

type family HasBearer xs :: Constraint where
  HasBearer (Bearer ': xs) = ()
  HasBearer (NamedBearer s ': xs) = ()
  HasBearer (JWT ': xs) = ()
  HasBearer (x ': xs)   = HasBearer xs
  HasBearer '[]         = BearerAuthNotEnabled

type family GetBearerHeaderName (xs :: [Type]) :: Symbol where
  GetBearerHeaderName (NamedBearer ('Just sym) ': xs) = sym
  GetBearerHeaderName (x ': xs) = GetBearerHeaderName xs
  GetBearerHeaderName '[] = "Authorization"

class BearerAuthNotEnabled

class CustomHeaderAuthNotEnabled

-- | @'HasBearer' auths@ is nominally a redundant constraint, but ensures we're not
-- trying to send a token to an API that doesn't accept them.
instance (KnownSymbol (GetBearerHeaderName auths), HasBearer auths, HasClient m api) => HasClient m (Auth auths a :> api) where
  type Client m (Auth auths a :> api) = Token -> Client m api

  clientWithRoute m _ req (Token token)
    = let headerName = toHeaderName $ symbolVal (Proxy @(GetBearerHeaderName auths))
      in clientWithRoute m (Proxy :: Proxy api)
        $ addSensitiveHeader headerName headerVal req
      where
        headerVal = Token $ "Bearer " <> token

        toHeaderName :: String -> CI BS.ByteString
        toHeaderName = mk . C8.pack

#if MIN_VERSION_servant_client_core(0,14,0)
  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl
#endif

-- * Authentication combinators

-- | A Bearer token in the Authorization header:
--
--    @Authorization: Bearer <token>@
--
-- This can be any token recognized by the server, for example,
-- a JSON Web Token (JWT).
--
-- Note that, since the exact way the token is validated is not specified,
-- this combinator can only be used in the client. The server would not know
-- how to validate it, while the client does not care.
-- If you want to implement Bearer authentication in your server, you have to
-- choose a specific combinator, such as 'JWT'.
data Bearer

data NamedBearer (mHeaderName :: Maybe Symbol)
