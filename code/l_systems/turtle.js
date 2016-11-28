<!-- <script> -->
"use strict";

function Turtle (canvas) {
    this.canvas = canvas;

    this.x = 0;
    this.y = 0;
    this.theta = 0;

    this.stack = [];
    this.canvas.translate(0, 150);
    this.canvas.scale(1, -1);
//    this.canvas.translate(25, 25);
//    this.canvas.transform(1, 0, 0, -1, 0, 0);

}


Turtle.prototype.turn = function (delta) {
    this.theta += delta;
    return this;
}

Turtle.prototype.push = function () {
    var state = [this.x, this.y, this.theta];
    this.stack.push(state);
    return this;
}

Turtle.prototype.pop = function () {
//    console.log("stack", this.stack);
    var state = this.stack.pop();
    this.x = state[0];
    this.y = state[1];
    this.theta = state[2];
    return this;
}

Turtle.prototype.moveTo = function (x, y) {
    this.x = x;
    this.y = y;
    return this;
}

Turtle.prototype.lineTo = function (x, y) {
    this.canvas.beginPath();
    this.canvas.moveTo(this.x, this.y);
    this.x = x;
    this.y = y;
    this.canvas.lineTo(this.x, this.y);
    this.canvas.closePath();
    this.canvas.stroke();
    return this;
}

Turtle.prototype.go = function (r) {
    var x = this.x + Math.cos(this.theta) * r;
    var y = this.y + Math.sin(this.theta) * r;
    
    this.lineTo(x, y);
    return this;
}

function apply_productions(productions, state) {
    var new_state = [];
    for(var i = 0; i < state.length; i++) {
        var out = productions[state[i]];
        if (out == null) {
            out = state[i];
        }
        for(var j = 0; j < out.length; j++) {
            new_state.push(out.charAt(j));
        }
    }
    return new_state.join('');
}

function to_turtle(turtle, state) {
    for(var i = 0; i < state.length; i++) {
        turtle.on_symbol(state[i]);
    }
}

//Attach axiom, productions
function LSystem () {
}

LSystem.prototype.draw = function (iters, turtle) {
    var cur = this.axiom;
    for(var i = 0; i < iters; i++) {
        cur = apply_productions(this.productions, cur);
    }
    to_turtle(turtle, cur);
}