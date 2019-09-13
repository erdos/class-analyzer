# bytecode-explorer

A disassemler for Java class files written in Clojure.

## Usage

FIXME

## Features

- dfs

## Is it any good?

Yes.


```
$ diff <(javap ) <(javap-clj)
...
```

In some scenarios it is even faster than javap.
```
time javap afddf
123ms
```

```
time afaf-adsfasfd
50ms
```

## Testing

Run all tests. This runs javap on all classes: `$ lein test :javap`

## License

Copyright (c) Janos Erdos. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/) which can be found in the file LICENSE.txt at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.

