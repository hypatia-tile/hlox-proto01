# hlox-proto01

By using Haskell, prototyping implementation of Lox language introduced in Crafting Interpreters[https://www.craftinginterpreters.com/contents.html]

## Usage

Move to the project directory and build the project using cabal.

```sh
cd hlox
cabal build
```
Execute directly from source code.

```sh
cabal run hlox -- <input_file>
```

`<input_file>` is the relative path from `hlox` directory to the Lox source code file.

If you want to execute Lox code interactively,
you can run the REPL by omitting the `<input_file>` argument.

```sh
cabal run hlox
```


```sh


