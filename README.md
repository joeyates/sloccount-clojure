# sloccount

Counts lines of code by programming language.

## Usage

From the shell:

```shell
cd sloccount-clojure
lein run /path/to/project
# => {:html 7758, :text 541, :xml 15830, :comment 280 ...}
```

From the repl:
```clojure
(require '[sloccount.core :as sloccount])
(sloccount/loc "/path/to/project")
;; => {:html 7758, :text 541, :xml 15830, :comment 280 ...}
```

## License

Copyright (c) 2009 Joe Yates, released under the MIT license

Modified 2013 by ken restivo 
