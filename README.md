# Zuse

<img align="left" src="resources/zuse_logo.svg" alt="original logo of the Zuse KG" width="200"/>
Zuse is a new programming language which aims to make writing lightning fast code
as easy as possible by providing extensible meta programming mechanisms to easily
verify programm correctness.

<br><br/>
<br><br/>
<br><br/>

## Getting started

### Installation

Make sure you have [Rust](https://www.rust-lang.org/) and [cargo-make](https://github.com/sagiegurari/cargo-make)
installed as this version of the compiler is written in Rust.
Then you can simple execute the following command.

```bash
cargo make install
```

After that you just have to add `$HOME/.zuse/bin` to your PATH. If you
are using [fish](https://fishshell.com/) or [bash](https://www.gnu.org/software/bash/)
you can simply execute one of the following commands.

#### If you are using bash
```bash
cargo make add-to-bashrc
```

#### If you are using fish
```bash
cargo make add-to-fish-config
```

## Syntax

#### A word of caution
These syntax examples do not represent the final syntax of the language they're
only supposed to give a sense of the general direction this language is going.
Because the syntax is not finished there is no support for highlighting codeblocks
in markdown. All the examples you see are using haskells's syntax highlighting.


### Metaprogramming
```haskell
-- // notice that our main function does not have to be named main. The #entry
-- // directive is used to annotate the entry of point of a zuse programm.
#entry
main :: () {
	foo := 42;
}
```

```haskell
-- // The function `foo` is going to run at compile time.
-- // It is not limited in any way compared to runtime code execution.
#run
foo :: () {
	msg := "Hello from the compile time world!";
	println!("{}", msg);
}
```


### Custom range types
```haskell
-- // Custom range types and a focus on compile time programm verification is 
-- // what sets Zuse apart from other programming languages

-- // every variable with the type `u8` has to be provable in the range from 0 to including 255
u8 :: 0..=255;

foo : u8 = 42; -- // this is fine
foo : u8 = 300; -- // compile time error

-- // this will also result in a compile time error
mut bar : u8 = 0;
for _ := 0..300 {
	bar += 1;
}
```

```haskell
-- // A max age of 140 years seems to be a pretty good estimate.
-- // At least according to wikipedia:
-- // https://en.wikipedia.org/wiki/List_of_the_verified_oldest_people
-- // So if a user of your software claims to be Elvis Presley and 200 years old something is probably not right :D
Age :: 0..=140;

-- // The #derive directive works pretty much like the one in rust 
#[derive(Debug, Clone, PartialEq, Eq)]
Person :: struct {
	name: String,
	age: Age,
}
```

### Enums and Generics
```haskell
-- // One of the best things in Rust are enums.
-- // Zuse enums are even more powerful and require less compiler magic to work.
Option :: enum($T) {
	Some($T),
	None,
}

-- // Enum inference through the dot operator allows for clear syntax without too much visual noise
foo := .Some(42);

-- // These cases are equivalent but discouraged.
foo := Option::Some(42);
foo : Option = Some(42);
```
