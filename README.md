# Gimlight

![30-05-2022_21:44:27_screenshot](https://user-images.githubusercontent.com/42336339/170999467-226e50c3-a325-4a0a-81aa-10bd9da841ea.png)

## Running

### Windows

We assume that you run all commands in this section on the [`msys2`](https://www.msys2.org/) terminal.

Install the dependencies with `pacman`:

```sh
pacman -S base-devel
pacman -S mingw-w64-x86_64-pkg-config
pacman -S mingw-w64-x86_64-SDL2
pacman -S mingw-w64-x86_64-glew
pacman -S mingw-w64-x86_64-freetype
```

After installing them, run this command on the project root:

```sh
cabal run gimlight
```

### Linux

You need to install `libsdl2` and `libglew`. For example, if you're using Ubuntu you need to run this command:

```sh
sudo apt install libsdl2-dev libglew-dev
```

After installing these dependencies, run this command on the project root:

```sh
LANG=C cabal run gimlight
```

`LANG=C` is necessary to prevent a build error. See [the c2hs' issue](https://github.com/haskell/c2hs/issues/238).

#### For Gentoo users

You need to enable the `opengl` use flag of `media-libs/libsdl2`:

```sh
sudo euse -E opengl -p media-libs/libsdl2
```

## License

All files in this repository **EXCEPT** any materials in the `third_party` directory are licensed under BSD 3-Clause License. For the licenses of the contents in the `third_party` directory, see the LICENSE files in each directory.
