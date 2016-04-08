# HaskQuest

/ˈhæs.kwɛst/

A Haskell-based domain-specific language for generating and playing text-based adventure games.

## Origin

HaskQuest (HQ) began as my final project for a functional programming course at the University of Utah taught by Dr Matt Flatt. I enjoy the challenges of parsing and interpreting and wanted to create a small language, and Dr Flatt suggested narrowing my language down to a specific goal.

## Feature List

Here's a list of things that I intend to complete at some point:

- [ ] Game Player (from text files)
  - Reads HQ code and creates an interactive game from it.
- [ ] Game Generator (from console input)
  - Parses an extended form of HQ which writes the HQ to file for playing later.
  - [ ] Compiler
    - Compiles HQ to a non-readable binary format to prevent players from tampering with game data.
    - Must be readable by the Game Player.
- [ ] Documentation
  - Every feature should be well-documented.

And I want HQ to support the following features for the actual adventure games themselves:

- [ ] Generate rooms
  - [ ] Description
  - [ ] Exits (transitions to other rooms)
  - [ ] Interactions (objects, items, NPCs, ...)
- [ ] Player inventory
  - [ ] Items
  - [ ] Descriptions
