# haboli

Haboli is a haskell library providing API bindings for
[the euphoria api](https://api.euphoria.io/). It can be used to create bots and
otherwise interact with the euphoria servers.

## Basic concept

This library is based around the custom `Client` monad. It is based on `IO` and
represents computations that happen while a connection to an euphoria server is
open. Once a `Client` finishes executing, the connection is automatically
closed. If the connection closes unexpectedly while the corresponding `Client`
is still running, it is notified and commands like `send` or `nick` will result
in an exception. The `Client` does not automatically reconnect.

The `Client` monad supports exceptions via the `throw`, `catch` and `handle`
operations, as well as multiple threads via the `fork` and `wait` operations. It
supports all session and chat room commands listed in the
[api reference](https://api.euphoria.io/).

For more information, see the haddock for the `Haboli.Euphoria.Client` and
`Haboli.Euphoria.Api` modules.

## Example bot

Here is a very basic example bot that Replies to `!ping` with `Pong!`:

```haskell
pingPongBot :: Client () ()
pingPongBot = forever $ do
  event <- respondingToPing nextEvent
  case event of
    EventSnapshot _ -> void $ nick "TreeBot"
    EventSend e ->
      let msg = sendMessage e
      in  when (msgContent msg == "!ping") $
            void $ reply msg "Pong!"
    _ -> pure ()
```

And here's how to run that bot:

```haskell
main :: IO ()
main = void $ runClient defaultConfig pingPongBot
```
