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

## Bots

The library is built with flexibility and composability in mind. Because of
this, there is no special `Bot` monad — bots also run inside the `Client` monad.
However, there are a few convenience modules that make development of bots
easier.

The convenience modules are built on top of the `Client` monad. None of the
convenience modules are necessary to create a functioning bot. When creating a
new bot, you can freely choose which modules to use and which to ignore or
replace with your own creations.

For an example bot structure using the convenience modules, here is an
[example bot](src/Haboli/Euphoria/ExampleBot.hs).

## Example client

Here is a very basic example bot that replies to `!ping` with `Pong!`. It does
not use any of the provided convenience modules.

```haskell
pingPongBot :: Client () ()
pingPongBot = forever $ do
  event <- respondingToPing nextEvent
  case event of
    EventSnapshot _ -> void $ nick "PingPongBot"
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

## Lenses

Haboli exports lenses for a few data types. The lenses are named like the record
accessors but suffixed with a `L`. For example, the lens corresponding to
`svNick` from `SessionView` is named `svNickL`. Lenses are not required to use
the libary. They are provided for the convenience of those who like using
lenses.
