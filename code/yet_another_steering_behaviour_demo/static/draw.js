<!-- <script> -->

Draw = function () {
    
function Interpolate(start, stop, object3d) {
    this.start = start;
    this.stop = stop;
//     console.log(this.start, this.stop);
    this.object3d = object3d;
}

Interpolate.prototype.move = function (alpha) {
    var pos = new THREE.Vector3(0, 0, 0);
    pos.addScaledVector(this.start, 1 - alpha);
    pos.addScaledVector(this.stop, alpha);
//     console.log(pos);
    this.object3d.position.copy(pos);
}

function Frame(deltas) {
    // console.log(deltas);
    this.interpolates = deltas.map(function (delta) {
        return new Interpolate(delta.start, delta.stop, delta.mesh);
    });
}

Frame.prototype.move = function (alpha) {
    // console.log(this.interpolates);
    this.interpolates.forEach(function (interpolate) {
        interpolate.move(alpha);
    });
}

// Delta
// has attributes
//     start: vector3
//     stop: vector3
//     mesh: object3d

function CircleDelta(start, stop, radius) {
    this.start = start;
    this.stop = stop;
    this.radius = radius;
    var geometry = new THREE.SphereGeometry(radius, 32, 32 );
    geometry.center();
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    this.mesh = new THREE.Mesh( geometry, material );
}

function Draw(width, height) {
    this.width = width;
    this.height = height;

    var scene = new THREE.Scene();
//     var camera = new THREE.PerspectiveCamera( 75, width/height, 0.1, 1000 );
    var camera = new THREE.OrthographicCamera( width / - 8, width / 8, height / 8, height / - 8, -5, 5 );

    camera.position.z = 5;

    var renderer = new THREE.WebGLRenderer();
    renderer.setSize( width, height );

    this.scene = scene;
    this.camera = camera;
    this.renderer = renderer;
}

Draw.prototype.get_element = function () {
    return this.renderer.domElement;
}

Draw.prototype.set_frame = function (deltas) {
    var self = this;
    this.frame = new Frame(deltas);
    
    for( var i = this.scene.children.length - 1; i >= 0; i--) { 
        this.scene.remove(this.scene.children[i]);
    }
    
    deltas.forEach(function (delta) {
        self.scene.add( delta.mesh );
    });
}

Draw.prototype.move = function (alpha) {
    this.frame.move(alpha);
}

Draw.prototype.mouse_to_world = function (x, y) {
    var vector = new THREE.Vector3();
    var far_vector = new THREE.Vector3();

    vector.set(
        ( x / this.width ) * 2 - 1,
        - ( y / this.height ) * 2 + 1,
        0.0 );
    
    
//     far_vector.set(
//         ( x / this.width ) * 2 - 1,
//         - ( y / this.height ) * 2 + 1,
//         1.0 );


    console.log("vector", vector);
    vector.unproject( this.camera );
//     far_vector.unproject( this.camera );
//     console.log("unprojected vector", vector, far_vector);
// 
//     var dir = vector.sub( this.camera.position ).normalize();
// 
//     var distance = - this.camera.position.z / dir.z;
// 
//     var pos = this.camera.position.clone().add( dir.multiplyScalar( distance ) );
//     console.log("mouse", pos);
    return vector;
}

return {
    Draw: Draw,
    CircleDelta: CircleDelta,
};
} ();


