import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Plant {
    constructor(parameters) {
        this.object = new THREE.Group();
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const plant = gltf.scene;
                plant.scale.copy(parameters.scale);
                plant.position.copy(parameters.position);
                if (parameters.rotation) {
                    plant.rotation.setFromVector3(parameters.rotation);
                }
                
                plant.traverse((child) => {
                    if (child.isMesh) {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(plant);
            },
            undefined,
            (error) => {
                console.error('Error loading plant:', error);
            }
        );
    }
}