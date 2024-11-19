import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';

export default class Doctor {
    constructor(parameters) {
        this.object = new THREE.Group(); // Certifique-se de inicializar como um THREE.Group

        const loader = new GLTFLoader();
        loader.load(
            parameters.url,
            (gltf) => {
                const doctor = gltf.scene;
                doctor.scale.copy(parameters.scale);
                doctor.position.copy(parameters.position);
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
            },
            undefined,
            (error) => {
                console.error('Error loading doctor:', error);
            }
        );
    }
}