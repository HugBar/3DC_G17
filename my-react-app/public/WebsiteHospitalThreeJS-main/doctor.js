import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Doctor {
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
                const doctor = gltf.scene;
                doctor.scale.copy(parameters.scale);
                if (parameters.rotation) {
                    doctor.rotation.setFromVector3(parameters.rotation);
                }
                
                doctor.traverse((child) => {
                    if (child.isMesh) {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(doctor);
                this.isLoaded = true;
                
                // Apply any pending visibility setting
                if (this.pendingVisibility !== null) {
                    this.setVisibility(this.pendingVisibility);
                    this.pendingVisibility = null;
                }
            },
            undefined,
            (error) => {
                console.error('Error loading doctor:', error);
            }
        );
    }

    setVisibility(visible) {
        if (!this.isLoaded) {
            this.pendingVisibility = visible;
            console.log('Doctor not loaded yet, setting pending visibility:', visible);
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
            console.log(`Set doctor visibility to ${visible} for ${meshCount} meshes at position:`, {
                x: this.object.position.x,
                z: this.object.position.z
            });
        }
    }
}