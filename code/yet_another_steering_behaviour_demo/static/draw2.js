<!-- <script> -->
"use strict";
var Draw = function () {    

function Draw(width, height) {
    this.width = width;
    this.height = height;

    this.actors = new Map();

    var scene = new THREE.Scene();
//     var camera = new THREE.PerspectiveCamera( 75, width/height, 0.1, 1000 );
    var camera = new THREE.OrthographicCamera( -100, 100, 100, -100, -5, 5 );

    camera.position.z = 5;

    var renderer = new THREE.WebGLRenderer();
    renderer.setSize( width, height );

    this.scene = scene;
    this.camera = camera;
    this.renderer = renderer;
}

Draw.prototype.add_actor = function (actor) {
    var geometry = new THREE.SphereGeometry(actor.get_radius(), 32, 32 );
    geometry.center();
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    var mesh = new THREE.Mesh( geometry, material );
    this.scene.add(mesh);
    this.actors.set(actor, mesh);
}

Draw.prototype.update = function () {
    this.actors.forEach(function (mesh, actor) {
        mesh.position.copy(actor.get_position());
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

return {
    Draw: Draw,
};
} ();


