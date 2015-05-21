

















-- Functional Webservices in Haskell
--
--          Alex Mason
--            NICTA































-- Let's build
--
--
--
--
--                Daintree
--     an Australian online book store
--
--
--
--
--
-- (All good book stores are named after the dead trees they're printed on)






















{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Book

import           Servant

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.Map.Strict            as M
import           Data.Monoid                ((<>))

import           Data.IORef

import           Network.Wai.Handler.Warp   (runEnv)



























-- GET      /books
-- GET      /book/<bookid>
-- POST     /book
-- DELETE   /book/<bookid>
--
-- -- If we have time (we probably won't)
-- GET      /book/<bookid>/cover.svg






















type Daintree =
        "books" :> Get '[JSON] [(Int,Book)]
            -- GET /books
            -- Accept-Type application/json
            --
            -- Handler type: EitherT ServantErr IO [(Int,Book)]

    :<|> "book" :> Capture "bookid" Int :> Get '[JSON] Book
            -- GET /book/<bookid>
            -- Accept-Type application/json
            --
            -- Handler type: Int        -> EitherT ServantErr IO Book

    :<|> "book" :> ReqBody '[JSON] Book :> Post '[JSON] (Int,Book)
            -- POST /foo
            -- body: {"authors":["Brian W. Kernighan"," Dennis M. Ritchie"],
            --        "isbn":"978-0131103627",
            --        "title":"The C Programming Language"}
            -- returns [<newid>,<json passed in>]
            --
            -- Handler type: Book           -> EitherT ServantErr IO (Int,Book)

    :<|> "book" :> Capture "bookid" Int :> Delete '[JSON] (Maybe Book)
            -- DELETE /book/<bookid>
            -- Accept-Type: application/json
            -- returns the object that's deleted or null if the key isn't found
            --
            -- Handler type: Int -> EitherT ServantErr IO (Maybe Book)

    -- :<|> "book" :> Capture "bookid" Int :> "cover.jpg" :> Get '[JPEG] Image

























-- Detour ==>






































type DB = IORef (Map Int Book)




































getAllBooks :: DB -> EitherT ServantErr IO [(Int,Book)]
getAllBooks ref = do
    books <- liftIO $ readIORef ref
    return $ M.assocs books
































getBook :: DB -> Int -> EitherT ServantErr IO Book
getBook ref bookid = do
    books <- liftIO $ readIORef ref
    case M.lookup bookid books of
        Nothing -> left err404
        Just book -> return book



































postBook :: DB -> Book -> EitherT ServantErr IO (Int, Book)
postBook ref book = liftIO $ atomicModifyIORef ref add

    where
        add books = case M.maxViewWithKey books of
            Nothing            -> (M.singleton 0 book, (0,book))
            Just ((maxid,_),_) ->
                let newmaxid = maxid+1
                in (M.insert newmaxid book books, (newmaxid, book))
































deleteBook :: DB -> Int -> EitherT ServantErr IO (Maybe Book)
deleteBook ref bookid = liftIO $ atomicModifyIORef ref del
    where
        del books = case M.splitLookup bookid books of
            (_,Nothing,_) -> (books,  Nothing)
            (l,Just x, r) -> (l <> r, Just x )





























bookMap :: Map Int Book
bookMap = M.fromList
    [(0,Book "Real World Haskell"
             ["Bryan O'Sullivan", "John Goerzen", "Donald Bruce Stewart"]
             "978-0-596-51498-3"
    )
    ,(1,Book "Command and Control"
             ["Eric Schlosser"]
             "978-0143125785"
    )
    ,(2, Book "Structure and Interpretation of Computer Programs - 2nd Edition"
              [ "Harold Abelson", "Gerald Jay Sussman", "Julie Sussman"]
              "978-0262510875"
    )
    ]

























-- Expanded type:
--
-- initBookServer :: IO (EitherT ServantErr IO [(Int, Book)]
--                 :<|> ((Int -> EitherT ServantErr IO Book)
--                 :<|> ((Book -> EitherT ServantErr IO (Int, Book))
--                 :<|> (Int -> EitherT ServantErr IO (Maybe Book)))))
--
-- (use :kind! Server Daintree in ghci to expand type families)

initBookServer :: IO (Server Daintree)
initBookServer = do
    ref <- newIORef bookMap

    return (getAllBooks ref :<|> getBook ref :<|> postBook ref :<|> deleteBook ref)





























bookServiceAPI :: Proxy Daintree
bookServiceAPI = Proxy


main :: IO ()
main = do
    bookHandlers <- initBookServer

    runEnv 8080 $ serve bookServiceAPI bookHandlers



-- serve :: HasServer api
--          => Proxy api
--          -> Server api
--          -> Application
--
-- Handles routing, currently by backtracking



























-- =============== Thanks =================
--
--            axman6@gmail.com
--
--      Alexander.Mason@nicta.com.au
--
-- Twitter: @axman6
-- IRC:      Axman6 -- Come join us at #haskell.au on Freenode
--
--
-- For more info, look at servant and servant-server on Hackage
--
-- Tutorial http://haskell-servant.github.io/tutorial
















