# Introduction to the Basic Game Structure Used by Keera Studios Ltd
Work in progress by Christina Zeller

## Goal of this document:
- We (Keera Studios) want to introduce (parts of) our recent basic game structure.
- Using this basic game structure as a skeleton in mind shall help you to easier and faster find your way in our games.
- Besides, if you are developing your own games, it might help you to structure your games.

## Reading advices and content of this document:
- [Warning](#warning)
  - Obviously everybody should read warnings.
    Here you find some limitations of the document.
- [Game Structure](#game-structure---the-modules)
  - If you want to know about our game structure, this is the place to look.
- [Why is a basic game structure helpful?](#why-is-a-basic-game-structure-helpful)
  - If you wonder whether you should know something about game structures, here you find some reasons why we think it is helpful.
- [Curious?](#curious)
  - If you want to get involved or use our material, take a look at this section.

## Warning
- This document describes parts of a theoretical (ideal) game structure. We are working hard to reach it, however sometimes reality clashes with theory. Our solution is either to work harder to get closer to the ideal or sometimes we are figuring out that our ideal is not ideal and we adapt the game structure.
- So don't be surprised if not all our games show the complete basic game structure or if in the future we change our mind and state another. That's called development.

## Game Structure - The Modules
- Basically all our games have the following modules.
- However, sometimes module names vary slightly. That's because we haven't found a consent regarding several names yet. Hopefully, this will become more stable someday in the near future.
- Therefore, if you have any suggestions to improve names or the structure, let us know!

```
  Game
      ----> Main
      ----> Constants
      ----> UserInput
      ----> DeviceOutput
      ----> InputOutputMatcher
      ----> Play
      ----> State
      ----> Objects
      ----> Levels
```

```
-------------------------------------------------------------------------------

                                                     Constants
Main
  |
  |___ init:
  |     UserInput
  |     DeviceOutput
  |
  |___ game loop:
       sense         (UserInput)    ---\  Input      / State (of Game)
       process input (Play) ------------> Output--->   Levels
       create output (DeviceOutput) ---/  Matcher    \ Objects

-------------------------------------------------------------------------------
```

### Main.hs
- Catch and print exceptions.
- Create handler for initializing input and output and allocate resources.
- Start the game loop. That is, sense input, process input, create output.

### Constants
Define magic numbers at one place with one name.

### DeviceOutput.hs
Handle output related  aspects, that is
 - Initialize device output related aspects like a rendering context, runtime context, window and sound.
 - Render (visual and audio).

### UserInput.hs
Provide controller for storing and recognizing user input.

### InputOutputMatcher
Match the user input with the displayed screen.

### State.hs
Provide the structure for game states.

###  Play.hs
Provide the game logic/flow.

###  Objects.hs
All objects of the game with specific properties and functions.

###  Levels.hs
Define every level with its characteristics.

## Why is a basic game structure helpful?
- The game structure helps to find your way in our games even if you are not familiar with the implementation details.
- If you handle many games you can change things in all games at the same time.
- Skeletons reduce needed brain capacity for maintenance.
  - It helps you to start to write your first game.
  - You can reuse code or outsource parts that are always handled in the same way.
  - Once well known, you can focus on the game specific parts not on the technical and maybe boring parts that need to be done in every game.
- Introducing new features will hopefully result in only minor changes of your game structure.
- ...

## Curious?

It seems that I caught your attention. If so let's get in contact ([issue](https://github.com/keera-studios/haskell-game-programming/issues) or [mail](chriz@keera.co.uk))! Some (but none exclusive) examples: Are you interested in ...
  * ... improving this document? Like:
    * Improving spelling and grammar.
    * Improving the comprehensibility of the document.
    * Improving the structure of the document.
    * ... ?
  * ... using this source for educational or other purpose?
  * ... letting me know what you think about this document?
  * ... writing your own Haskell game?
  * ...?
