# bytecode-explorer

A disassemler for Java Class files written in Clojure.



## Usage

Run on a class file from the Clojure source code.


```
lein run -- -c  clojure/asm/TypePath.class 
```

You can check that the output is the same as with javap:

```
javap -c clojure.asm.TypePath
```


## Testing

Run unit tests with `$ lein test` command.

Run javap comparison tests with: `$ lein test :javap`

## License

Copyright (c) Janos Erdos. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/) which can be found in the file LICENSE.txt at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.

