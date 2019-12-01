# bytecode-explorer

A disassembler library for Java Class files.

The purpose of the project is to provide an idiomatic Clojure framework for
disassembling Java Class files based on the [Java Virtual Machine Specification](https://docs.oracle.com/javase/specs/jvms/se7/html/index.html).

It is also a fun topic to learn about the quirks and oddities of one of the most
significant codebase in the modern software ecosystem.

The program is validated by running it on a selection of class files on the
developer's `~/.m2` folder. For testing, an output layer is provided that renders
in the same format as `javap` does. It is expected to give the very same output
as the `javap` command. It is just like a reimplementation of the standard Java
disassembler written in Clojure.

![Written in Clojure](https://img.shields.io/github/languages/top/erdos/class-analyzer)
[![Hello Visitors](http://hits.dwyl.io/erdos/class-analyzer.svg)](http://hits.dwyl.io/erdos/class-analyzer)

## Usage

You can embed it as a library in your project, or run it as a standalone program: `lein run [ARGS] [CLASSES]`

Arguments:

- `-c`: prints bytecode of methods (like in `javap`)
- `-l`: prints line numbers and local variable tables (like in `javap`)
- `--x-edn`: prints output as a nice [EDN](https://github.com/edn-format/edn) data structure instead of text.

For example, to run it on a class file from the Clojure source code.


```
lein run -- -c  clojure/asm/TypePath.class
```

You can check that the output is the same as with javap:

```
javap -c clojure.asm.TypePath
```


## Testing

You will need [Leiningen](https://leiningen.org/) to run the tests.

- Run unit tests with `$ lein test` command.
- Run javap comparison tests with: `$ lein test :javap`

## License

Copyright (c) Janos Erdos. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/) which can be found in the file LICENSE.txt at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.
