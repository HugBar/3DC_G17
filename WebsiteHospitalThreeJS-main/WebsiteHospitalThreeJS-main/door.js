import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Door {
    constructor(parameters) {
        this.object = new THREE.Group();
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const door = gltf.scene;
                door.scale.copy(parameters.scale);
                door.position.copy(parameters.position);
                if (parameters.rotation) {
                    door.rotation.setFromVector3(parameters.rotation);
                }
                
                door.traverse((child) => {
                    if (child.isMesh) {
                        child.material = new THREE.MeshPhongMaterial({ 
                            color: parameters.color || 0x8B4513,  // Default to brown if no color specified
                            shininess: 30,
                            specular: 0x444444
                        });
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(door);
            },
            undefined,
            (error) => {
                console.error('Error loading door:', error);
            }
        );
    }
}