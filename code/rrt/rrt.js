
<!-- <script> -->
var RRT = function () {
var Tree = function (initial) {
    this.vertices = new Set([initial]);
    this.edges = new Map();
}

Tree.prototype.add_vertex = function (v) {
    this.vertices.add(v);
}

Tree.prototype.add_edge = function (src, dst) {
    this.edges.set(dst, src);
}

var nearest_vertex = function(tree, vertex, distance_f) {
    var nearest;
    var nearest_distance = Infinity;
    tree.vertices.forEach(function (v) {
        var distance = distance_f(vertex, v);
        if (nearest_distance > distance) {
            nearest_distance = distance;
            nearest = v;
        }
    });
    return nearest;
}

var RRT = function (initial, distance_f, rand_conf, new_conf) {
    this.tree = new Tree(initial);
    this.distance_f = distance_f;
    this.rand_conf = rand_conf;
    this.new_conf = new_conf;
}

RRT.prototype.step = function () {
    var rand_vertex = this.rand_conf();
    var nearest = nearest_vertex(this.tree, rand_vertex, this.distance_f);
    var new_vertex = this.new_conf(nearest, rand_vertex);
    this.tree.add_vertex(new_vertex);
    this.tree.add_edge(nearest, new_vertex);
<!--     console.log(new_vertex); -->
}

return {
    RRT: RRT
};
} ();
   



    