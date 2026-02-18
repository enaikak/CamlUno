# CamlUno

A terminal-based implementation of the UNO card game written in **OCaml**.

## Features
- 2–4 player gameplay with turn ordering and direction changes  
- Special card effects: skip, reverse, draw-two, wild, wild +4  
- ANSI-colored card rendering in the terminal  
- Modular game engine with separated logic and UI components  
- Property-based testing using **QCheck**

## How to Run

From the repository root:

```bash
cd uno
dune build
dune exec bin/main.exe
