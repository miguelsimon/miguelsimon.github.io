---
layout: post
comments: true
title: "Lindenmayer systems"
meta: "Lindenmayer systems in javascript"
date: 2016-11-11
tags: []
---

<script src="/code/l_systems/turtle.js"></script>

A [Lindenmayer system](https://en.wikipedia.org/wiki/L-system) is a formalism for describing [rewriting systems](https://en.wikipedia.org/wiki/Rewriting_system) that's surprisingly useful for compactly specifying, among other things, plant morphology.

If we want to use an L-system to describe morphology, we need:

 The grammar that specifies the rewriting rules: to specify the grammar, we need
    an alphabet of possible symbols
    a set of production rules that map symbols to successors
    the initial string that we start off with

 The visualizer that takes a string produced by the L-system and outputs a visualization.

### The grammar

We're going to implement *deterministic* grammars in javascript; the alphabet will be implicit in our production rules and axiom string.

#### Production rules

We will represent our deterministic production rules as a javascript dictionary; using the Pythagoras treee,

```javascript
productions = {
    '1': '11',
    '0': '1[0]0'
}
```

#### Axiom

We will represent an axiom string (indeed, any string) as a simple javascript string, in the algae case:

```javascript
axiom = '0';
```

### The visualizer

We need a way to map the string representation of the system to a geometric one: I'm going to implement a tiny [turtle graphics](https://en.wikipedia.org/wiki/Turtle_graphics)-like system in javascript using the HTML 5 canvas element. Code for that is [here](/code/l_systems/turtle.js).

Given this system, we simply need to write a translator that takes a symbol string and emits drawing commands.

<canvas id="pythagoras_canvas" width="150" height="150"></canvas>

<script>
"use strict";

var canvas = document.getElementById("pythagoras_canvas");
var ctx = canvas.getContext("2d");

var lsystem = new LSystem();


lsystem.productions = {
    '1': '11',
    '0': '1[0]0'
}

lsystem.axiom = "0";

var turtle = new Turtle(ctx);

turtle.moveTo(75, 0);
turtle.turn(Math.PI / 2);

turtle.on_symbol = function (symbol) {
    console.log("symbol", symbol);
    switch (symbol) {
        case "0":
            this.go(10);
            break;
        case "1":
            this.go(10);
            break;
        case "[":
            this.push()
                .turn(Math.PI / 4);
            break;
        case "]":
            this.pop()
                .turn(-Math.PI / 4);
            break;
        default:
            console.log("bad symbol", symbol);
    }
}

lsystem.draw(4, turtle);


</script>

Ok, so that worked. Let's try the Sierpinski gasket:

<canvas id="sierpinski_canvas" width="150" height="150"></canvas>

<script>
"use strict";

var canvas = document.getElementById("sierpinski_canvas");
var ctx = canvas.getContext("2d");

var lsystem = new LSystem();


lsystem.productions = {
    'F': 'F-G+F+G-F',
    'G': 'GG'
}

lsystem.axiom = "F-G-G";

var turtle = new Turtle(ctx);

//turtle.moveTo(75, 0);
turtle.turn(Math.PI / 2);

turtle.on_symbol = function (symbol) {
    console.log("symbol", symbol);
    switch (symbol) {
        case "F":
            this.go(8);
            break;
        case "G":
            this.go(8);
            break;
        case "+":
            this.turn(2 * Math.PI / 3);
            break;
        case "-":
            this.turn(- 2 * Math.PI / 3);
            break;
        default:
            console.log("bad symbol", symbol);
    }
}

lsystem.draw(4, turtle);


</script>

And now the dragon curve:

<canvas id="dragon_canvas" width="300" height="300"></canvas>

<script>
"use strict";

var canvas = document.getElementById("dragon_canvas");
var ctx = canvas.getContext("2d");

var lsystem = new LSystem();


lsystem.productions = {
    'X': 'X+YF+',
    'Y': '-FX-Y'
}

lsystem.axiom = "FX";

var turtle = new Turtle(ctx);

turtle.moveTo(75, 0);
//turtle.turn(Math.PI / 2);

turtle.on_symbol = function (symbol) {
    console.log("symbol", symbol);
    switch (symbol) {
        case "F":
            this.go(8);
            break;
        case "Y":
            break;
        case "X":
            break;
        case "+":
            this.turn(2 * Math.PI / 4);
            break;
        case "-":
            this.turn(- 2 * Math.PI / 4);
            break;
        default:
            console.log("bad symbol", symbol);
    }
}

lsystem.draw(9, turtle);


</script>

Now for the wiki's last example, the fractal plant:

<canvas id="plant_canvas" width="800" height="600"></canvas>

<script>
"use strict";

var canvas = document.getElementById("plant_canvas");
var ctx = canvas.getContext("2d");

var lsystem = new LSystem();


lsystem.productions = {
    'X': 'F-[[X]+X]+F[+FX]-X)',
    'F': 'FF'
}

lsystem.axiom = "X";

var turtle = new Turtle(ctx);

turtle.moveTo(0, -100);
//turtle.turn(Math.PI / 2);

turtle.on_symbol = function (symbol) {
    console.log("symbol", symbol);
    switch (symbol) {
        case "F":
            this.go(8);
            break;
        case "X":
            break;
        case "+":
            this.turn(2 * Math.PI / 14.4);
            break;
        case "-":
            this.turn(- 2 * Math.PI / 14.4);
            break;
        case "[":
            this.push();
            break;
        case "]":
            this.pop();
            break;
        default:
            console.log("bad symbol", symbol);
    }
}

lsystem.draw(5, turtle);


</script>


