import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Chair {
    constructor(parameters) {
        this.object = new THREE.Group();
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const chair = gltf.scene;
                chair.scale.copy(parameters.scale);
                chair.position.copy(parameters.position);
                if (parameters.rotation) {
                    chair.rotation.setFromVector3(parameters.rotation);
                }
                
                chair.traverse((child) => {
                    if (child.isMesh) {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(chair);
            },
            undefined,
            (error) => {
                console.error('Error loading chair:', error);
            }
        );
    }
}