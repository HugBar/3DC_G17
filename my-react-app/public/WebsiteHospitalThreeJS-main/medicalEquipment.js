import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class MedicalEquipment {
    constructor(parameters) {
        this.object = new THREE.Group();
        this.roomPosition = {
            x: parameters.position.x - 1, // Adjust based on room size
            y: parameters.position.y,
            z: parameters.position.z
        };
        this.roomDimensions = {
            width: 2, // Adjust based on your room size
            height: 2
        };
        this.camera = parameters.camera;
        
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
                        child.userData.clickable = true;
                        child.userData.type = 'medicalEquipment';
                        child.userData.parent = this;
                    }
                });
                
                this.object.add(equipment);
            }
        );
    }

    selectRoom(camera) {
        // Update camera reference
        console.log('Selecting room');
        this.camera = camera;
        
        if (!this.camera || !this.cameraPosition) return;
        
        // Move camera to predefined position
        this.camera.position.set(
            this.cameraPosition.x,
            this.cameraPosition.y,
            this.cameraPosition.z
        );
        
        // Look at room center
        const roomCenter = {
            x: this.roomPosition.x + (this.roomDimensions.width / 2),
            y: 0,
            z: this.roomPosition.z
        };
        
        this.camera.lookAt(roomCenter.x, roomCenter.y, roomCenter.z);
    }
}