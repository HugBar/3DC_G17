import * as THREE from "three";

export default class Lights {
    constructor(parameters) {
        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        // Create a group of objects
        this.object = new THREE.Group();

        // Create the ambient light
        this.object.ambientLight = new THREE.AmbientLight(this.ambientLight.color, this.ambientLight.intensity);
        this.object.add(this.object.ambientLight);

        // Create directional light
        this.object.directionalLight = new THREE.DirectionalLight(this.directionalLight.color, this.directionalLight.intensity);
        this.object.directionalLight.position.copy(this.directionalLight.position);
        this.object.directionalLight.castShadow = true;

        // Configure shadow properties
        this.object.directionalLight.shadow.mapSize.width = 2048;
        this.object.directionalLight.shadow.mapSize.height = 2048;
        this.object.directionalLight.shadow.camera.near = 0.1;
        this.object.directionalLight.shadow.camera.far = 100;
        this.object.directionalLight.shadow.camera.left = -15;
        this.object.directionalLight.shadow.camera.right = 15;
        this.object.directionalLight.shadow.camera.top = 15;
        this.object.directionalLight.shadow.camera.bottom = -15;

        this.object.add(this.object.directionalLight);
    }
}