# diamondtau
CS 3110 Final Project 
- Ronit Totlani rt526
- Enaika Kishnani ek756
- Nathan Li nl549

GenAI Usage: ChatGPT helped with the logic for obtaining the index of the next player in clockwise and counterclockwise order using mod. ChatGPT helped with the logic for replace_range for displaying the top of the discard pile, as well as understanding ANSI escape codes for coloring the card. It also helped with understanding how to structure and use QCheck, the Fisher-Yates shuffle algorithm, and general test writing strategies. It also helped with writing docstrings for functions.

## Testing

This project uses both example-based unit tests (OUnit2) and property-based tests (QCheck).

- Run the build:
```bash
dune build
```

- Run the full test suite (OUnit + QCheck):
```bash
dune runtest
```

- Run the QCheck test executable directly:
```bash
dune exec ./test/test_qcheck.exe -- --seed <seed>
```