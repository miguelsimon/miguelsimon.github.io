<!-- <script> -->
"use strict";
var World = function () {

function World () {
    this.actors = new Set();
}

World.prototype.forEach_vehicle = function (f) {
    this.actors.forEach(f);
}

World.prototype.add_actor = function (actor) {
    this.actors.add(actor);
}

World.prototype.rem_actor = function (actor) {
    this.actors.delete(actor);
}

World.prototype.physics_step = function (dt) {
    this.actors.forEach(function (actor) {
        actor.p.addScaledVector(actor.v, dt);
        actor.p.setZ(0);
        actor.v.setZ(0);
    });

}

return {
    World: World,
}
} ();