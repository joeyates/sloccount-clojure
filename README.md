# sloccount

Counts lines of code.

## Usage

From the shell:

```shell
cd sloccount-clojure
lein run  /path/to/project
# => {:html 7758, :text 541, :xml 15830, :comment 280 ...}
```

From the repl:
```clojure
(require '[sloccount.core :as sloccount])
(sloccount/loc "/path/to/project")
;; => {:html 7758, :text 541, :xml 15830, :comment 280 ...}
```

## TO DO
- Load settings from file
- Allow user configuration: command line and rc file
    - Ignore paths
	- User-defined file types
- Use state machine to count:
    - ruby: block comments and erb
    - perl: block comments
- Make a lein plugin out of this?


## License

Copyright (c) 2009 Joe Yates, released under the MIT license

Modified 2013 by ken restivo 
