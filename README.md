# repl-color

![screenshot](https://raw.github.com/razum2um/repl-color/master/doc/screenshot.png)

## Usage

    (require 'repl-color.core)
    ;; and just use clojure.pprint/pprint

If you want to revert default behaviour

    (use 'clojure.pprint)
    (set-pprint-dispatch simple-dispatch)

## License

Copyright © 2014 Vlad Bokov

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
