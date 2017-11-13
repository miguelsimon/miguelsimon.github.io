---
layout: post
comments: true
title: "Yet another steering behaviours demo in javascript and three.js"
meta: "A few simple steering behaviours implemented in javascript and visualized using three.js"
date: 2016-10-31
tags: [motion_planning]
---

<script src="https://threejs.org/build/three.js"></script>
<script src="/code/yet_another_steering_behaviour_demo/static/demo.js"></script>

I wanted to familiarize myself with the excellent [three.js](https://threejs.org/) library.

A long time ago I was captivated by the powerfully simple ideas behind [boids](https://en.wikipedia.org/wiki/Boids), so I'm going to implement some of Craig Reynold's [steering behaviours](http://www.red3d.com/cwr/steer/gdc99/) in javascript.


### Seek

The seek behaviour goes for its target at maximum speed. Of course, this will lead to oscillations when the vehicle reaches the target (click to provide a target):

<div id="seek_canvas"></div>

<script>
"use strict";

var seek_vehicle = new Demo.Vehicle(
    new THREE.Vector3(0, 0, 0),
    new THREE.Vector3(0, 0, 0));

seek_vehicle.move = function (world) {
    var desired_v = this.target.clone();
    desired_v.sub(this.p);
    desired_v.normalize();
    desired_v.multiplyScalar(this.max_speed);
    
    var steering = desired_v.clone();
    steering.sub(this.v);
    steering.clampLength(0, this.max_impulse);
    this.v.add(steering);
}

var seek_demo = new Demo.DemoWindow(document.getElementById("seek_canvas"), 600, 400);
seek_demo.add_vehicle(seek_vehicle);

</script>

### Arrive

Arrival behaviour is similar to seek, but it ramps down its velocity as it approaches its target:

<div id="arrive_canvas"></div>
<script>
var arrive_vehicle = new Demo.Vehicle(
    new THREE.Vector3(0, 0, 0),
    new THREE.Vector3(0, 0, 0));
    
arrive_vehicle.move = function (world) {
    var desired_v = this.target.clone();
    desired_v.sub(this.p);
    var distance = desired_v.length();
    var ramped_speed = this.max_speed * (distance / 100);
    var speed = Math.min(ramped_speed, this.max_speed);
    desired_v.normalize();
    desired_v.multiplyScalar(speed);
    var steering = desired_v.clone();
    steering.sub(this.v);
    steering.clampLength(0, this.max_impulse);
    this.v.add(steering);
}

var arrive_demo = new Demo.DemoWindow(document.getElementById("arrive_canvas"), 600, 400);

arrive_demo.add_vehicle(arrive_vehicle);
</script>

### Wander

Wander moves around aimlessly.

<div id="wander_canvas"></div>
<script>
var wander_demo = new Demo.DemoWindow(document.getElementById("wander_canvas"), 600, 400);
for (var i = 0; i < 5; i++) {
var wander_vehicle = new Demo.Vehicle(
    new THREE.Vector3(0, 0, 0),
    new THREE.Vector3(0, 0, 0));
    
wander_vehicle.move = function (world) {
    if (! this.wander_rads) {
        this.wander_rads = 0;
    }
    var dir = 1;
    if (Math.random() > 0.5) dir = -1;
    this.wander_rads += dir *  Math.PI * 0.05;
    var seek_point = this.forward.clone();
    seek_point.multiplyScalar(5);

    var euler = new THREE.Euler( 0, 0, this.wander_rads, 'XYZ' );
    var desired_v = new THREE.Vector3(1, 0, 0);
    
    desired_v.applyEuler(euler);

    desired_v.add(seek_point);
    desired_v.sub(this.v);
    
    desired_v.clampLength(0, this.max_impulse * 0.05);
    this.v.add(desired_v);
}


wander_demo.add_vehicle(wander_vehicle);
}
</script>
