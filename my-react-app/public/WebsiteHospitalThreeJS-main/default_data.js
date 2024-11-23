import * as THREE from "three";
import Orientation from "./orientation.js";

export const generalData = {
    setDevicePixelRatio: false
}

export const mazeData = {
    url: "./mazes/Loquitas.json",
    credits: "Maze designed by Cecília Fernandes and Nikita.",
    scale: new THREE.Vector3(1.0, 1.0, 1.0)
}

export const playerData = {
    url: "./models/gltf/RobotExpressive/RobotExpressive.glb",
    credits: "Model and related code snippets created by <a href='https://www.patreon.com/quaternius' target='_blank' rel='noopener'>Tomás Laulhé</a>. CC0 1.0. Modified by <a href='https://donmccurdy.com/' target='_blank' rel='noopener'>Don McCurdy</a>.",
    eyeHeight: 0.8, // fraction of character height
    scale: new THREE.Vector3(0.1, 0.1, 0.1),
    walkingSpeed: 5,
    initialDirection: 0.0, // Expressed in degrees
    turningSpeed: 75.0, // Expressed in degrees / second
    runningFactor: 2.0, // Affects walking speed and turning speed
    keyCodes: { fixedView: "Digit1", firstPersonView: "Digit2", thirdPersonView: "Digit3", topView: "Digit4", viewMode: "KeyV", userInterface: "KeyU", miniMap: "KeyM", help: "KeyH", statistics: "KeyS", run: "KeyR", left: "ArrowLeft", right: "ArrowRight", backward: "ArrowDown", forward: "ArrowUp", jump: "KeyJ", yes: "KeyY", no: "KeyN", wave: "KeyW", punch: "KeyP", thumbsUp: "KeyT", interact: "KeyE", door: "KeyE" },
}

export const lightsData = {
    ambientLight: { color: 0xffffff, intensity: 0.5 },
    directionalLight: { 
        color: 0xffffff, 
        intensity: 1.0,
        position: new THREE.Vector3(5, 10, 5)
    }
}

export const fogData = {
    enabled: false,
    color: 0xe0e0e0,
    near: 0.1,
    far: 14.0
}

export const cameraData = {
    view: "fixed", // Fixed view: "fixed"; first-person view: "first-person"; third-person view: "third-person"; top view: "top"; mini-map: "mini-map"
    multipleViewsViewport: new THREE.Vector4(0.0, 0.0, 1.0, 1.0), // Viewport position and size: fraction of window width and window height; MUST BE REDEFINED when creating an instance of ThumbRaiser() so that each view is assigned a different viewport
    target: new THREE.Vector3(0.0, 0.0, 0.0), // Target position
    initialOrientation: new Orientation(135.0, -45.0), // Horizontal and vertical orientation and associated limits (expressed in degrees)
    orientationMin: new Orientation(-180.0, -90.0),
    orientationMax: new Orientation(180.0, 0.0),
    initialDistance: 8.0, // Distance to the target and associated limits
    distanceMin: 4.0,
    distanceMax: 16.0,
    initialZoom: 1.0, // Zoom factor and associated limits
    zoomMin: 0.5,
    zoomMax: 2.0,
    initialFov: 45.0, // Field-of-view (expressed in degrees)
    near: 0.01, // Front clipping plane
    far: 100.0 // Back clipping plane
}

export const chairData = [
    {
        url: "./models/gltf/Chairs/Chair.glb",
        credits: "Hospital chair model",
        scale: new THREE.Vector3(0.4, 0.4, 0.4),
        position: new THREE.Vector3(-4.675, 0, 1.5),  
        rotation: new THREE.Vector3(0, Math.PI / -2, 0)  

    },
    {
        url: "./models/gltf/Chairs/Chair.glb",
        credits: "Hospital chair model",
        scale: new THREE.Vector3(0.4, 0.4, 0.4),
        position: new THREE.Vector3(-4.675, 0, 3),  
        rotation: new THREE.Vector3(0, Math.PI / -2, 0)  
    },
    {
        url: "./models/gltf/Chairs/Chair.glb",
        credits: "Hospital chair model",
        scale: new THREE.Vector3(0.4, 0.4, 0.4),
        position: new THREE.Vector3(-7.35, 0, 1.5),  
        rotation: new THREE.Vector3(0, Math.PI / 2, 0)  
    },
    {
        url: "./models/gltf/Chairs/Chair.glb",
        credits: "Hospital chair model",
        scale: new THREE.Vector3(0.4, 0.4, 0.4),
        position: new THREE.Vector3(-7.35, 0, 3),  
        rotation: new THREE.Vector3(0, Math.PI / 2, 0)  
    },
]

export const plantData = [
    {
        url: "./models/gltf/Plant/Plant.glb",
        credits: "Hospital plant model",
        scale: new THREE.Vector3(0.5, 0.5, 0.5),
        position: new THREE.Vector3(-7.225, 0.1, 3.75),  // Position in corner
        rotation: new THREE.Vector3(0, 0, 0)
    },
    {
        url: "./models/gltf/Plant/Plant.glb",
        credits: "Hospital plant model",
        scale: new THREE.Vector3(0.5, 0.5, 0.5),
        position: new THREE.Vector3(-4.80, 0.1, 3.75),  // Position in corner
        rotation: new THREE.Vector3(0, 0, 0)
    }

]

export const medicalEquipmentData = [
    {
        url: "./models/gltf/MedicalEquipment/MedicalEquipment.glb",
        credits: "Hospital medical equipment model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(0.5, 0, 3.15),  // First room
        rotation: new THREE.Vector3(0, Math.PI / -1, 0) // rotate 180 degrees around Y axis
    },

    {
        url: "./models/gltf/MedicalEquipment/MedicalEquipment.glb",
        credits: "Hospital medical equipment model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(-2.50, 0, 3.15),  // First room
        rotation: new THREE.Vector3(0, Math.PI / -1, 0) // rotate 180 degrees around Y axis
    },

    {
        url: "./models/gltf/MedicalEquipment/MedicalEquipment.glb",
        credits: "Hospital medical equipment model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(3.50, 0, 3.15),  // First room
        rotation: new THREE.Vector3(0, Math.PI / -1, 0) // rotate 180 degrees around Y axis
    },
    {
        url: "./models/gltf/MedicalEquipment/MedicalEquipment.glb",
        credits: "Hospital medical equipment model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(6.50, 0, 3.15),  // First room
        rotation: new THREE.Vector3(0, Math.PI / -1, 0) // rotate 180 degrees around Y axis
    },
    {
        url: "./models/gltf/MedicalEquipment/MedicalEquipment.glb",
        credits: "Hospital medical equipment model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(5.50, 0, -3.15),  // First room
        rotation: new THREE.Vector3(0, 0, 0) // rotate 180 degrees around Y axis
    },

]
export const patientData = [
    {
        url: "./models/gltf/Patient/patient.glb",
        credits: "Hospital patient model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(-2.30, 0.4, 3.40),  // Position on the bed
        rotation: new THREE.Vector3(0, Math.PI / -2, 0)  // Rotate to face the right direction
    },

    {
        url: "./models/gltf/Patient/patient.glb",
        credits: "Hospital patient model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(0.70, 0.4, 3.40),  // Position on the bed
        rotation: new THREE.Vector3(0, Math.PI / -2, 0)  // Rotate to face the right direction
    },
    {
        url: "./models/gltf/Patient/patient.glb",
        credits: "Hospital patient model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(6.70, 0.4, 3.40),  // Position on the bed
        rotation: new THREE.Vector3(0, Math.PI / -2, 0)  // Rotate to face the right direction
    },
    {
        url: "./models/gltf/Patient/patient.glb",
        credits: "Hospital medical equipment model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(3.70, 0.4, 3.40),  // First room
        rotation: new THREE.Vector3(0, Math.PI / -2, 0) // rotate 180 degrees around Y axis
    },
    

    // Você pode adicionar mais pacientes seguindo o mesmo padrão
];
export const doctorData = [
    {
        url: "./models/gltf/Doctor/Doctor.glb",
        credits: "Hospital doctor model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(6.50, 0, 3.15),  // OR-101
        rotation: new THREE.Vector3(0, Math.PI / 0.5, 0)
    },
    {
        url: "./models/gltf/Doctor/Doctor.glb",
        credits: "Hospital doctor model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(3.50, 0, 3.15),  // OR-102
        rotation: new THREE.Vector3(0, Math.PI / 0.5, 0)
    },
    {
        url: "./models/gltf/Doctor/Doctor.glb",
        credits: "Hospital doctor model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(0.50, 0, 3.15),  // OR-103
        rotation: new THREE.Vector3(0, Math.PI / 0.5, 0)
    },
    {
        url: "./models/gltf/Doctor/Doctor.glb",
        credits: "Hospital doctor model",
        scale: new THREE.Vector3(0.005, 0.005, 0.005),
        position: new THREE.Vector3(-2.50, 0, 3.15),  // OR-104
        rotation: new THREE.Vector3(0, Math.PI / 0.5, 0)
    }
];
export const deskData = [
    {
        url: "./models/gltf/Desk/Desk.glb",
        credits: "Hospital desk model",
        scale: new THREE.Vector3(0.0032, 0.0032, 0.0032),
        position: new THREE.Vector3(-5.46, 0, -2.35),
        rotation: new THREE.Vector3(0, Math.PI / 2, 0)
    }
];
export const vendingMachineData = [
    {
        url: "./models/gltf/VendingMachine/VendingMachine.glb",
        credits: "Hospital vending machine model",
        scale: new THREE.Vector3(0.75, 0.75, 0.75),
        position: new THREE.Vector3(-7.0, 0, -3.50),
        rotation: new THREE.Vector3(0, 0, 0)
    }
];


export const doorData = [

];
