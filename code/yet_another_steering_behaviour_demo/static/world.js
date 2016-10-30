<!-- <script> -->
"use strict";
var World = function () {

function circle_collision(p1, r1, p2, r2) {
    var distance = p1.distanceTo(p2);
    return distance < r1 + r2;
}

function Actor(x, y, r) {
    this.p = new THREE.Vector3(x, y, 0);
    this.v = new THREE.Vector3(0, 0, 0);
    this.r = r;
}

Actor.prototype.circle_collision = function (p, r) {
    return circle_collision(this.p, this.r, p, r);
}

// Actor.prototype.halfplane_collision = function(p, normal) {
//     return false;
// }
function World () {
    this.actors = new Set();
}

World.prototype.add_actor = function (actor) {
    this.actors.add(actor);
}

World.prototype.rem_actor = function (actor) {
    this.actors.delete(actor);
}

World.prototype.physics_step = function (dt) {
//     this.collision_resolution();
    this.actors.forEach(function (actor) {
        var t_0 = actor.p.clone();
        actor.p.addScaledVector(actor.v, dt);
        actor.p.setZ(0);
        actor.v.setZ(0);
        var t_1 = actor.p.clone();
    });

}

return {
    Actor: Actor,
    World: World,
}
} ();