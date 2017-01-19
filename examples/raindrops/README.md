I've used this game in several talks to demonstrate that game programming in
Haskell is very, very easy. If you attended any of those talks and listened to
this boring fellow, thank you all for attending the talk :)

The slides for one of those talks are here:
http://ivanperez-keera.github.io/slides/2015-04-upm-iperez/#/

I would recommend going through them again, in particular, to see the videos
from youtube. These also explain why your game does not have to be 3D or
complex to be good or successful. The last few videos are from games that
became (at least for a few weeks) hits on Google Play.

A few pointers to help you on your way:

# Haskell programming
- Learn you a Haskell for great good (Miran Lipovaca) (http://learnyouahaskell.com/chapters)
- Introduction to Functional Programming (Bird & Wadler) (old edition).

# Functional Reactive Programming
- Functional Reactive Animation (Elliott & Hudak, 1997). I would recommend
  reading the Intro, and section 3 for larger examples. In section 2, I would
skip the first part of section 2.1 if it becomes too hard to understand.
- http://conal.net/fran/tutorial.htm Contains multiple examples with
  animations.
- The Yampa Arcade (Courtney, Nilsson & Peterson, 2003) describes FRP based on
  Signal Functions in a very clear and concised way. It then uses it to create
a small asteroids game.
- Yampa (Functional Reactive Programming implementation):
  https://github.com/ivanperez-keera/Yampa
- Elm (try FRP on your browser): http://elm-lang.org/try,
  http://elm-lang.org/Examples.elm

# Game Programming
- Game Engine Architecture (Jason Gregory). (This book does not contain code. Instead, it tries to give you a good understanding of what game programming entails, the problems you will find, and good solutions. I personally think it's extremely good.)

# Game Design
- The Art of Game Design: A book of lenses (Jesse Schell, ex-Disney, ex-Bell
  Labs, currently Carneige Mellon and CEO of Schell Games, a game development
company with more than 100 employees).

# Examples of games written in Haskell
- Nikki and the Robots. https://github.com/nikki-and-the-robots/nikki 
- Haskanoid: http://github.com/ivanperez-keera/haskanoid (This game uses wiimotes, kinect and mouse.)
- Raincat: https://github.com/styx/Raincat
- Nario bros: https://github.com/keera-studios/monao
- Frag (3D, OpenGL): https://github.com/snowmantw/Frag (Note: this game is written in Yampa, a Functional Reactive Programmming framework)
- Falling Blocks (SDL): https://github.com/ivanperez-keera/fallingblocks (Note: this game is a good example of what one could do in 700 lines of Haskell. I would change a few things about the architecture, for instance, I would include an abstract Controller/Input, and separate rendering and logic game state, but the code is still quite clear.)
- Voldemort: an addictive, quick-paced arcade game
  https://github.com/ivanperez-keera/zurihac2015

# Writing Haskell games for Android
On linux, just run the steps described here:
https://github.com/neurocyte/ghc-android

It will create a chroot within which you will have a haskell compiler ready to
create Android code. It takes some time to compile, but it should work
perfectly. From that, creating your first hello world and running it on your
phones should be trivial.

When you have your Haskell game running on desktop and want to distribute it
for Android, drop me an email and I'll help you get it running. You'll need to
create an Android project, a Java class and a small C program, but I can send
you mine.

Recommendation: In your games, do not use print/putStrLn, or your code won't
work well on Android.

# Libraries

SDL (Simple DirectMedia Layer) is a collection of libraries that provides basic
operations to do multimedia, networking, threading, etc. I recommend that you
use it for your games, unless you need 3D.

SDL 1.2 and OpenGL are available on hackage. To install them, just:
    $ cabal install OpenGL
and
    $ cabal install sdl sdl-image sdl-mixer sdl-ttf

For SDL2, I use the following repositories instead of what is available by
default:
- https://github.com/keera-studios/hsSDL2.git
- https://github.com/jdeseno/hs-sdl2-image
- https://github.com/jdeseno/hs-sdl2-mixer
- https://github.com/osa1/hsSDL2-ttf/
- https://github.com/keera-studios/hssdl2-gfx

# Assets
- OpenGameArt: http://opengameart.org/
- DaFont (only for fonts): http://www.dafont.com/

# Online communities
- Facebook: https://www.facebook.com/groups/programming.haskell/

# Other notes
Whenever you try new things using cabal, use sandboxes. Sandboxes help you keep
your environment clean, by installing packages only in one directory instead of
making them available for every user/haskell project.

It's simple:
```
mkdir myproject
cd myproject
cabal sandbox init
cabal install ...<whatever>...
```
