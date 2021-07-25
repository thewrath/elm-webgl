# Little Space shooter to discover Elm WebGL ✨

This repository presents a program written with the [Elm language](https://elm-lang.org) which aims to show that Elm is perfectly adapted for game development.

It uses :
- ✔️ Elm programming language
- ✔️ WebGL as rasterization API (for rendering)
- Audio ?
- Text rendering ?
- Tmx loader ?

Very few Elm libraries have been added (except elm-webgl) to show that Elm contains everything that is needed for GD.

## Improvements
Many improvements can be made to the code: :

- Mesh management is not very flexible and duplicate code is required for each type of mesh (textured or colored)
- The `Player`, `Enemy` and `Bullet` modules have a lot of duplicate code that could be pooled into the `Entity` module.
	- The problem here is to achieve polymorphism/inheritance/composition with Elm Records.

## Todo 
- Use nested Entity in Player module (like in Enemy module)
- texture region to load spritesheet instead of single sprite