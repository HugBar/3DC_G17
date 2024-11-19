import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class MedicalEquipment {
    constructor(parameters) {
        this.object = new THREE.Group();
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const equipment = gltf.scene;
                equipment.scale.copy(parameters.scale);
                equipment.position.copy(parameters.position);
                if (parameters.rotation) {
                    equipment.rotation.setFromVector3(parameters.rotation);
                }
                
                equipment.traverse((child) => {
                    if (child.isMesh) {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(equipment);
            },
            undefined,
            (error) => {
                console.error('Error loading medical equipment:', error);
            }
        );
    }
}