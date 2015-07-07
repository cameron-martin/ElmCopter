# ElmCopter

This is an implementation of the classic flash game [Copter][copter] written in [Elm][elm].

It is no way near done, but the basic idea is there. The obstacle course is the same each time, and since the obstacle generation is pretty naive, at some time it will get impossible.

After coming back to this after a few months, I noticed the code was really hard to grok, so it needs some refactoring.

## Usage

1. [Install elm][elm_install]
2. Compile using

        elm make Main.elm --output Main.html
    
3. Open `Main.html` in your browser

[copter]: http://www.coptergame.net/
[elm]: http://elm-lang.org/
[elm_install]: http://elm-lang.org/install