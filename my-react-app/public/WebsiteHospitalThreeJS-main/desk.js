import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Desk {
    constructor(parameters) {
        this.object = new THREE.Group();
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const desk = gltf.scene;
                desk.scale.copy(parameters.scale);
                desk.position.copy(parameters.position);
                if (parameters.rotation) {
                    desk.rotation.setFromVector3(parameters.rotation);
                }
                
                desk.traverse((child) => {
                    if (child.isMesh) {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(desk);
            },
            undefined,
            (error) => {
                console.error('Error loading desk:', error);
            }
        );
    }
}