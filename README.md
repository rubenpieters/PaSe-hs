<p align="center">
  <img width="334" height="298" src="logo.png">
</p>

> PaSe (`/peɪs/`) is a Haskell animation library based on compositional animation design. The basic building blocks are **pa**rallel and **se**quential composition of animations.


# Quick Start

In PaSe you build animations in a compositional fashion. This means that we start from *atomic* animation elements and build larger animations with *combinators*.

## Atomic Animations

Atomic animations are the indivisible units of animation which specify how the application state should change over a period of time. A simple example is the `linearTo` animation, which changes a property over a specified duration to a target value. The property is specified with a [lens](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html), the duration in seconds and the target value with a Float.

 For example, `linearTo (player . x) (For 1) (To 300)` moves the player's `x` value to `300` over a duration of 1 second.

![LinearTo](https://raw.githubusercontent.com/rubenpieters/PaSe-hs/master/pictures/linearto.gif)

## Combinators

Parallel composition expresses that two animations should start playing at the same time. For example, ``moveX `parallel` moveSheet`` will play both the `moveX` and `moveSheet` animation at the same time.

![Parallel](https://raw.githubusercontent.com/rubenpieters/PaSe-hs/master/pictures/parallel.gif)

Sequential composition expresses that after the first animation is done playing, the second animation will start playing. For example, ``swing `sequential` minusOne`` will first play the `swing` animation and after that the `minusOne` animation.

![Sequential](https://raw.githubusercontent.com/rubenpieters/PaSe-hs/master/pictures/sequential.gif)

# Demos and Examples

Examples and demos are inside the `PaSe-examples/` folder.

# Assets

Assets used in the demo application.

- background: https://edermunizz.itch.io/free-pixel-art-forest
- player: https://rvros.itch.io/animated-pixel-hero
- slime: https://rvros.itch.io/pixel-art-animated-slime
