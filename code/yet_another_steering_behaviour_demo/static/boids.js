<!-- <script> -->

draw = new Draw.Draw(window.innerWidth, window.innerHeight);

document.body.appendChild( draw.get_element() );

function Vehicle(actor, max_speed, target) {
    this.actor = actor;
    this.max_speed = max_speed;
    this.max_impulse = 5;
    this.target = target;
    
    this.neighborhood_radius = 100;
    this.min_separation = 5;   
}

Vehicle.prototype.get_position = function () {
    return this.actor.p;
}

Vehicle.prototype.set_target = function(v) {
    this.target.copy(v);
}

Vehicle.prototype.move = function (vehicles) {
    var neighbors = this.get_neighbors(vehicles);
//    console.log("neighbors", neighbors);

    var separation_steer = this.separation(neighbors);
    var alignment_steer = this.alignment(neighbors);
    var cohesion_steer = this.cohesion(neighbors);
    var target_steer = this.target_steer();

    var steering = new THREE.Vector3(0, 0, 0);
    
//    console.log("separation", separation_steer);
//    console.log("alignment", alignment_steer);
//    console.log("cohesion", cohesion_steer);
//    console.log("target", target_steer);

    steering.add(separation_steer);
    steering.add(alignment_steer);
//   steering.add(cohesion_steer);
    steering.add(target_steer);

    steering.normalize();
    steering.multiplyScalar(this.max_impulse);

    this.actor.v.add(steering);
    this.actor.v.multiplyScalar(0.9); //damping
}

Vehicle.prototype.target_steer = function () {
    var desired_v = this.target.clone();
    desired_v.sub(this.actor.p);
    desired_v.normalize();
    desired_v.multiplyScalar(this.max_speed);
    steering = desired_v.clone();
    steering.sub(this.actor.v);
    steering.normalize();
    steering.multiplyScalar(0.1);
    return steering;
}

Vehicle.prototype.separation = function (neighbors) {
    var f = new THREE.Vector3(0, 0, 0);
    var p = this.actor.p;
    neighbors.forEach(function (n) {
        var diff = p.clone();
        diff.sub(n.actor.p);
        var length2 = diff.lengthSq();
        diff.normalize();
        f.addScaledVector(diff, 1 / length2)
    });
    
    return f;
}

Vehicle.prototype.alignment = function (neighbors) {
    var v = new THREE.Vector3(0, 0, 0);
    neighbors.forEach(function (n) {
        v.add(n.actor.v);
    });
    v.add(this.actor.v);
    v.divideScalar(1 + neighbors.length);
    v.sub(this.actor.v);
    v.normalize();
    v.multiplyScalar(0.1);
    return v;
}

Vehicle.prototype.cohesion = function (neighbors) {
    var c = new THREE.Vector3(0, 0, 0);
    neighbors.forEach(function (n) {
        c.add(n.actor.p);
    });
    c.add(this.actor.p);
    c.divideScalar(1 + neighbors.length);
    var dist = c.distanceTo(this.actor.p);
    var factor = dist / 10;
    c.sub(this.actor.p);
    c.normalize();
    c.multiplyScalar(factor * factor);
    return c;
}

Vehicle.prototype.get_neighbors = function (vehicles) {
    var neighbors = [];
    var p = this.actor.p;
    var self = this;
    var neighborhood_radius = this.neighborhood_radius;
    vehicles.forEach(function (other) {
        if (self != other && p.distanceTo(other.actor.p) < neighborhood_radius) {
            neighbors.push(other);
        }
    })
    return neighbors;
}
    
    
var world = new World.World();

var vehicles = [];
var target = new THREE.Vector3(0, 0, 0);
var max_speed =100;
for(var i = 0; i < 10; i++) {
    var vehicle = new Vehicle(new World.Actor(Math.random() * 50, Math.random() * 50), max_speed, target);
    vehicles.push(vehicle);
}

vehicles.forEach(function (vehicle) {
    world.add_actor(vehicle.actor);
    draw.add_actor(vehicle);
});

draw.get_element().addEventListener("click", function (event) {
    console.log('click');
    console.log(event.clientX, event.clientY, draw.mouse_to_world(event.clientX, event.clientY));
//     console.log(vehicle.actor.p);
 
    vehicles.forEach(function (vehicle) {
        vehicle.set_target(draw.mouse_to_world(event.clientX, event.clientY));
        console.log(vehicle.actor.p);
    });
});

var step = function () {
    vehicles.forEach(function (vehicle) {
        vehicle.move(vehicles);
    });
    world.physics_step(0.1);
}

var render = function (timestamp) {

    requestAnimationFrame( render );
    step();
    draw.update();
    draw.renderer.render(draw.scene, draw.camera);

};

render(performance.now());