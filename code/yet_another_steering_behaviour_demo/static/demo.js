<!-- <script> -->
"use strict";
var Demo = function () {
    
//Toy physics world

function World (bounding_box) {
    this.bounding_box = bounding_box;
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
    var bounding_box = this.bounding_box;
    this.actors.forEach(function (actor) {
        if (actor.is_obstacle) {
            return;
        }
        
        //Euler forward integration
        actor.p.addScaledVector(actor.v, dt);
        actor.p.setZ(0);
        actor.v.setZ(0);
        
        //calculate heading
        if (actor.v.length() > 0) {
            actor.forward.copy(actor.v);
            actor.forward.normalize();
        }
        
        //reset position and velocity when actor leaves world bounds
        if ( ! bounding_box.intersectsSphere(actor.get_bounding_sphere()) ) {
            actor.p.set(0, 0, 0);
            actor.v.set(0, 0, 0);
            console.log("set", actor.p, actor.v);
        }
        
    });
}

// Very simple drawing interface using three.js 
function Draw(width, height) {
    this.width = width;
    this.height = height;

    this.actors = new Set();

    var scene = new THREE.Scene();
    var camera = new THREE.OrthographicCamera( -width / 2, width / 2, height / 2, -height / 2, -5, 5 );

    camera.position.z = 5;

    var renderer = new THREE.WebGLRenderer();
    renderer.setSize( width, height );

    this.scene = scene;
    this.camera = camera;
    this.renderer = renderer;
}

Draw.prototype.add_actor = function (actor) {
    var mesh = actor.get_mesh();
    this.scene.add(mesh);
    this.actors.add(actor);
}

Draw.prototype.update = function () {
    this.actors.forEach(function (actor) {
        actor.update_graphics();
    });
}

Draw.prototype.get_element = function () {
    return this.renderer.domElement;
}

Draw.prototype.mouse_to_world = function (x, y) {
    var vector = new THREE.Vector3();

    vector.set(
        ( x / this.width ) * 2 - 1,
        - ( y / this.height ) * 2 + 1,
        0.0 );

    console.log("vector", vector);
    vector.unproject( this.camera );

    return vector;
}
    
    
// Utility for making independent demo contexts to be able to inline them
function DemoWindow(target_element, width, height) {
    
    var draw = new Draw(width, height);
    target_element.appendChild( draw.get_element() );
        
    var bounding_box = new THREE.Box3(
        new THREE.Vector3(-width/2, -height/2, -10),
        new THREE.Vector3(width/2, height/2, 10)
    );
    var world = new World(bounding_box);

    var step = function (dt) {
        world.forEach_vehicle(function (vehicle) {
            vehicle.move();
        });
        world.physics_step(dt);
    }

    var render = function (timestamp) {
        requestAnimationFrame( render );
        step(0.1);
        draw.update();
        draw.renderer.render(draw.scene, draw.camera);
    };

    render();
    
    draw.get_element().addEventListener("click", function (event) {
        world.forEach_vehicle(function (vehicle) {
            vehicle.set_target(draw.mouse_to_world(event.offsetX, event.offsetY));
        });
    });
    
    this.add_vehicle = function (vehicle) {
        world.add_actor(vehicle);
        draw.add_actor(vehicle);
    }
}

// A simple vehicle (we'll attach move() functions to them to modify their behaviour)
function Vehicle(p, v) {
    this.p = p;
    this.v = v;
    this.r = 4;
    
    this.forward = new THREE.Vector3(1, 0, 0);
    
    this.max_speed = 50;
    this.max_impulse = 5;
    this.target = new THREE.Vector3(0, 0, 0);
    
    var geometry = new THREE.CylinderGeometry( 0, 5, 15, 3 );
    geometry.rotateX(Math.PI / 2);
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    this.mesh = new THREE.Mesh( geometry, material );
    
    this.bounding_sphere = new THREE.Sphere(
        this.p,
        this.r);
}

Vehicle.prototype.update_graphics = function () {
    this.mesh.position.copy(this.p);
    var look = this.p.clone();
    look.add(this.forward);
    this.mesh.lookAt(look);
}

Vehicle.prototype.get_mesh = function () {
    return this.mesh;
}

Vehicle.prototype.set_target = function(v) {
    this.target.copy(v);
}

Vehicle.prototype.get_bounding_sphere = function () {
    return this.bounding_sphere;
}

function Obstacle(p, r) {
    this.p = p;
    this.r = r;
    this.is_obstacle = true;
    
    var geometry = new THREE.SphereGeometry( r );
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    this.mesh = new THREE.Mesh( geometry, material );
    
    this.bounding_sphere = new THREE.Sphere(
        this.p,
        this.r);
}

Obstacle.prototype.update_graphics = function () {
    this.mesh.position.copy(this.p);
}

Obstacle.prototype.get_mesh = function () {
    return this.mesh;
}

Obstacle.prototype.get_bounding_sphere = function () {
    return this.bounding_sphere;
}

return {
    World: World,
    Draw: Draw,
    DemoWindow: DemoWindow,
    Vehicle: Vehicle,
    Obstacle: Obstacle
};
} ();