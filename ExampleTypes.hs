
















-- Server is a type family which converts between the API type and the
-- types of the handlers needed for handling all requests defined by the API:


Server API -> APIHandlerTypes




































:: Server (Get '[JSON] Bar)
    ==>
    EitherT ServantErr IO Bar
                -- Handler returns a Foo
                -- Servant takes care of turning it into JSON



































:: Server ("path" :> Get '[JSON] Bar)
    ==>
    EitherT ServantErr IO Bar
        -- same as above, path pieces are ignored
        -- by the handler - you can change path of
        -- a handler with changing the handler itself






























:: Server ("path" :> Get '[JSON, PlainText, JPEG] Bar)
    ==>
    EitherT ServantErr IO Bar
        -- Servant can handle multiple different Accept-Types
        -- Supported types are listed in the type level list
        -- encodes the returned Bar as JSON, PlainText or a
        -- JPEG image depending on client request.




































:: Server (Capture '[JSON] Foo :> Get '[JSON] Foo)
    ==>
    Foo -> EitherT ServantError IO Foo
        -- Capture says that your handler must accept an
        -- argument of the defined type, in this case a Foo.
        -- Servant takes care of decoding the values in the URL

































:: Server (ReqBody '[JSON,PlainText] Bar :> Get '[JSON] Foo)
    ==> Bar -> EitherT ServantError IO Foo
        -- Can specify the encoding and type of the request body,
        -- Servant takes care of decoding the body for you
        -- Handler won't be called if it can't be decoded































:: Server (a :<|> b)
    ==>
    Server a :<|> Server b


:: Server ("foo" :> Get '[JSON] Foo :<|> "bar" :> Get '[JSON] Bar)
    ==>
    Server ("foo" :> Get '[JSON] Foo) :<|> Server ("bar" :> Capture '[JSON] Book :> Get '[JSON] Bar)
    ==>
    EitherT ServantErr IO Foo :<|> Book -> EitherT ServantErr IO Bar
































-- Also support for:
QueryParam
QueryParams
QueryFlag

"foo" :> QueryParam "offset" Int :> Get ... -- /foo?offset=10

MatrixParam
MatrixParams
MatrixFlag

"foo" :> MatrixParam "bigfoo" Bool :> "bar" :> Get .. -- /foo;true/bar
                                                      -- Apparently this is a thing

Header: "foo" :> Header "E-Tag" Text :> Get ... -- /foo where the the E-Tag header has been set

Raw: -- Allows you  to use Wai `Application`s





















