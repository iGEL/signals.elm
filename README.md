## Signals!

This is a toy project to learn [Elm](http://elm-lang.org/). It renders some
German railway signals as SVG, because I wanted to learn them as well.

![](https://media1.giphy.com/media/l1KVb2dUcmuGG4tby/giphy.gif)

![](/signals.png)

### Setup

To be written :scream:

### Usage in your own project

The project is still being developed, so it can't yet be used from JavaScript
directly. But you can use the signals from Elm.

Initialize a any kind of signal, call one of the initializers: 

```elm
SignalModel.distantSignal : SignalModel.Model
SignalModel.signalRepeater : SignalModel.Model
SignalModel.combinationSignal : SignalModel.Model
SignalModel.mainSignal : SignalModel.Model

-- distant signal, a repeater, a combination signal and a main signal in a record
signals = { distantSignal = SignalModel.distantSignal
          , signalRepeater = SignalModel.signalRepeater
          , combinationSignal = SignalModel.combinationSignal
          , mainSignal = SignalModel.mainSignal
          }
```

Any of those signals can be displayed with calling

```elm
Signal.view : SignalModel.Model -> Svg msg

-- for example:
Signal.view signals.distantSignal
```

For setting an aspect of the signal or configure certain things, have a look at
the available messages in [src/Messages.elm](/src/Messages.elm). Because the
combination signal has both a distant as well main state, you have to tell the
update function, which one you want to update (also for the distant or main
signals).

Send the all messages both to the main signal and all corresponding distant
signals. This way they will show the correct distant aspect for the main
signal.

```elm
Signal.update : Messages.Target -> SignalModel.Model -> SignalModel.Model

-- for example:
signals =
    { signals
        | combinationSignal =
            Signal.update
                (ToDistantSignal Proceed)
                signals.combinationSignal
        , mainSignal =
            Signal.update
                (ToMainSignal Proceed)
                signals.mainSignal
    }
```
