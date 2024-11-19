import * as THREE from "three";

/*
 * parameters = {
 *  textureUrl: String
 * }
 */

export default class Wall {
    constructor(parameters) {
        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        // Create a texture
        const texture = new THREE.TextureLoader().load(this.textureUrl);
        texture.colorSpace = THREE.SRGBColorSpace;
        
        // Improve texture quality
        texture.anisotropy = 16; // Increases sharpness of the texture at different angles
        texture.magFilter = THREE.LinearFilter;
        texture.minFilter = THREE.LinearMipmapLinearFilter;
        
        // Enable texture repeat
        texture.wrapS = THREE.RepeatWrapping;
        texture.wrapT = THREE.RepeatWrapping;
        // Repeat texture to make it more detailed
        texture.repeat.set(2, 4); // Adjust these values to change texture density

        // Create a simple wall using BoxGeometry
        const geometry = new THREE.BoxGeometry(1, 2.5, 0.1); // width, height, depth
        const material = new THREE.MeshPhongMaterial({ 
            color: 0xffffff,
            map: texture,
            bumpScale: 0.5,    // Adds depth to the texture
            normalScale: new THREE.Vector2(1, 1) // Enhances surface detail
        });

        // Create the wall mesh
        this.object = new THREE.Mesh(geometry, material);
        
        // Enable shadows
        this.object.castShadow = true;
        this.object.receiveShadow = true;

        // Adjust the wall position to account for the increased height
        this.object.position.y = 1.25; // Half of the height to center it
    }
}