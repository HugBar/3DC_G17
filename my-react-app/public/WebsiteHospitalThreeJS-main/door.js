import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Door {
    constructor(parameters) {
        this.object = new THREE.Group();
        this.isOpen = false;
        this.isAnimating = false;
        this.rotationAngle = 0;
        this.targetRotation = 0;
        this.animationSpeed = 0.02;
        this.lastToggleTime = 0;
        this.initialRotation = parameters.rotation.y || Math.PI;
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                this.door = gltf.scene;
                this.door.scale.copy(parameters.scale);
                this.door.position.copy(parameters.position);
                if (parameters.rotation) {
                    this.door.rotation.setFromVector3(parameters.rotation);
                }
                
                this.door.traverse((child) => {
                    if (child.isMesh) {
                        child.material = new THREE.MeshPhongMaterial({ 
                            color: parameters.color || 0x8B4513,
                            shininess: 30,
                            specular: 0x444444
                        });
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(this.door);
            },
            undefined,
            (error) => {
                console.error('Error loading door:', error);
            }
        );
    }

    toggleDoor() {
        const currentTime = Date.now();
        if (!this.isAnimating && currentTime - this.lastToggleTime > 1000) {
            this.isAnimating = true;
            this.targetRotation = this.isOpen ? this.initialRotation : this.initialRotation - Math.PI / 2;
            this.isOpen = !this.isOpen;
            this.lastToggleTime = currentTime;
        }
    }

    update() {
        if (this.isAnimating && this.door) {
            const diff = this.targetRotation - this.rotationAngle;
            if (Math.abs(diff) > 0.001) {
                this.rotationAngle = THREE.MathUtils.lerp(
                    this.rotationAngle,
                    this.targetRotation,
                    this.animationSpeed
                );
                this.door.rotation.y = this.rotationAngle;
            } else {
                this.isAnimating = false;
                this.rotationAngle = this.targetRotation;
                this.door.rotation.y = this.rotationAngle;
            }
        }
    }
}