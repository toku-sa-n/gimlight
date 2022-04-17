# Gimlight

## Running

### Windows

We assume that you run all commands in this section on the [`msys2`](https://www.msys2.org/) terminal.

Install the dependencies with `pacman`:

```sh
sudo pacman -S base-devel
sudo pacman -S mingw-w64-x86_64-pkg-config
sudo pacman -S mingw-w64-x86_64-SDL2
sudo pacman -S mingw-w64-x86_64-glew
sudo pacman -S mingw-w64-x86_64-freetype
```

After installing them, run this command on the project root:

```sh
cabal run gimlight
```

### Linux

You need to install `libsdl2` and `libglew`. For Ubuntu you need to run this command:

```sh
sudo apt install libsdl2-dev libglew-dev
```

After installing these dependencies, run this command on the project root:

```sh
LANG=C cabal run
```

`LANG=C` is necessary to prevent a build error. See [the c2hs' issue](https://github.com/haskell/c2hs/issues/238).

## License

All files in this repository **EXCEPT** any materials in the `third_party` directory are licensed under BSD 3-Clause License. For the licenses of the contents in the `third_party` directory, see the LICENSE files in each directory.
