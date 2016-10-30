<script>

draw = new Draw.Draw(window.innerWidth, window.innerHeight);

document.body.appendChild( draw.get_element() );

function Vehicle(actor, max_speed, target) {
    this.actor = actor;
    this.max_speed = max_speed;
    this.max_impulse = 5;
    this.target = target;
}

Vehicle.prototype.set_target = function(v) {
    this.target.copy(v);
}

Vehicle.prototype.move = function (world) {
    var desired_v = this.target.clone();
    desired_v.sub(this.actor.p);
    desired_v.normalize();
    desired_v.multiplyScalar(this.max_speed);
    steering = desired_v.clone();
    steering.sub(this.actor.v);
    steering.normalize();
    steering.multiplyScalar(this.max_impulse);
    this.actor.v.add(steering);
    this.actor.v.multiplyScalar(0.9);
//    console.log('step');
//    console.log(this.actor.p);
//    console.log(this.actor.v);
//   console.log(this.target);
}
    
    
var world = new World.World();
var vehicle = new Vehicle(new World.Actor(50, 50), 100, new THREE.Vector3(50, 50));
world.add_actor(vehicle.actor);

draw.get_element().addEventListener("click", function (event) {
    console.log('click');
    console.log(event.clientX, event.clientY, draw.mouse_to_world(event.clientX, event.clientY));
//     console.log(vehicle.actor.p);
    
    //vehicle.set_target(draw.mouse_to_world(event.clientX, event.clientY));
});

var step = function () {
    vehicle.move(world);
    var deltas = world.physics_step(0.1);
    var frame = deltas.map(function (d) {
        return new Draw.CircleDelta(
            d[0],
            d[1],
            1);
    });
    return frame;
}

var start;
var render = function (timestamp) {
    if (!start) start = timestamp;
    var progress = timestamp - start;
//    console.log(progress); 
    var alpha = progress / 1000;
    if (progress < 1000) {
        requestAnimationFrame( render );
        draw.move(alpha);
        draw.renderer.render(draw.scene, draw.camera);
    } else {
        var frame = step();
//         console.log(frame);
        draw.set_frame(frame);
        start = null;
        render(performance.now());
    }
};


draw.set_frame([]);
render(performance.now());