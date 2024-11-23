import * as THREE from "three";
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class VendingMachine {
    constructor(parameters) {
        this.object = new THREE.Group();
        
        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const vendingMachine = gltf.scene;
                vendingMachine.scale.copy(parameters.scale);
                vendingMachine.position.copy(parameters.position);
                if (parameters.rotation) {
                    vendingMachine.rotation.setFromVector3(parameters.rotation);
                }
                
                vendingMachine.traverse((child) => {
                    if (child.isMesh) {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                });
                
                this.object.add(vendingMachine);
            },
            undefined,
            (error) => {
                console.error('Error loading vending machine:', error);
            }
        );
    }
}

