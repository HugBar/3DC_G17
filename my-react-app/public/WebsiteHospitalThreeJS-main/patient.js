import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Patient {
    constructor(parameters) {
        this.object = new THREE.Group();
        this.isLoaded = false;
        this.pendingVisibility = null;
        
        // Set the group's position immediately
        this.object.position.copy(parameters.position);
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const patient = gltf.scene;
                patient.scale.copy(parameters.scale);
                // Don't set position on the patient model since it's already set on the group
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
                this.isLoaded = true;
                
                // Apply any pending visibility setting
                if (this.pendingVisibility !== null) {
                    this.setVisibility(this.pendingVisibility);
                    this.pendingVisibility = null;
                }
            },
            undefined,
            (error) => {
                console.error('Error loading patient:', error);
            }
        );
    }
    
    setVisibility(visible) {
        if (!this.isLoaded) {
            this.pendingVisibility = visible;
            console.log('Patient not loaded yet, setting pending visibility:', visible);
            return;
        }
        
        if (this.object) {
            let meshCount = 0;
            this.object.traverse((child) => {
                if (child.isMesh) {
                    child.visible = visible;
                    meshCount++;
                }
            });
            this.object.visible = visible;
            console.log(`Set visibility to ${visible} for ${meshCount} meshes at position:`, {
                x: this.object.position.x,
                z: this.object.position.z
            });
        }
    }

}