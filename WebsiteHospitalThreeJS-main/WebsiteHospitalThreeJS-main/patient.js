import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Patient {
    constructor(parameters) {
        this.object = new THREE.Group();
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const patient = gltf.scene;
                patient.scale.copy(parameters.scale);
                patient.position.copy(parameters.position);
                if (parameters.rotation) {
                    patient.rotation.setFromVector3(parameters.rotation);
                }
                
                patient.traverse((child) => {
                    if (child.isMesh) {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(patient);
            },
            undefined,
            (error) => {
                console.error('Error loading patient:', error);
            }
        );
    }
}