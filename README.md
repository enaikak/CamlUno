# CamlUno

A terminal-based implementation of the UNO card game written in **OCaml**.

## Features
- 2–4 player gameplay with turn ordering and direction changes  
- Special card effects: skip, reverse, draw-two, wild, wild +4  
- ANSI-colored card rendering in the terminal  
- Modular game engine with separated logic and UI components  
- Property-based testing using **QCheck**

## Commands
- `play <idx>` — play the card at position `<idx>`
- `draw` — draw a card and end your turn
- `help` — show the help menu
- `quit` — exit the game

## Project Structure
- `uno/bin/` — main terminal game loop  
- `uno/lib/` — core game logic and card types  
- `uno/test/` — unit + property-based tests  

## Notes
Developed as a team project for CS 3110 and published here as a personal portfolio copy.

## How to Run
From the repository root:

```bash
cd uno
dune build
dune exec bin/main.exe
