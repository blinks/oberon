# Oberon

Algorithmic musical composition.

## Installation

    $ git clone https://github.com/blinks/oberon
    $ cd oberon && lein deps

## Usage

Oberon expects a MIDI out port to be available for sending messages.  I
generally use MIDI Patchbay (free for OSX) to hook it up to Reason.  If
you have an external poly synth available, that'd work well too.

    $ java -jar oberon-0.1.0-standalone.jar [args]

## Options

None yet.

## Examples

* [Opus 1](http://soundcloud.com/hackerblinks/oberon-opus-1)
* [Opus 2](http://soundcloud.com/hackerblinks/oberon-opus-2)

### Bugs

* Notes sometimes are too short due to rhythmic strangeness.
* Key modulation sometimes overreaches.
* The piece doesn't generally have enough of an arc.
* The code is a mess.

## License

Copyright © 2013 Adam Blinkinsop

Distributed under the Eclipse Public License, the same as Clojure.
