class RoomInfo {
    constructor() {
        this.visible = false;
        this.currentRoom = null;
        this.overlay = this.createOverlay();
        
        // Informações estáticas das salas
        this.roomsData = {
            'OR-101': {
                id: 'OR-101',
                status: "Ocupada",
                type: "Sala de Cirurgia",
                capacity: 4,
                assignedDoctor: "Dr. Silva",
                assignedPatient: "João Santos"
            },
            'OR-102': {
                id: 'OR-102',
                status: "Disponível",
                type: "Sala de Cirurgia",
                capacity: 4,
                assignedDoctor: "Não atribuído",
                assignedPatient: "Não atribuído"
            },
            'OR-103': {
                id: 'OR-103',
                status: "Ocupada",
                type: "Sala de Cirurgia",
                capacity: 4,
                assignedDoctor: "Dra. Costa",
                assignedPatient: "Maria Oliveira"
            },
            'OR-104': {
                id: 'OR-104',
                status: "Em Limpeza",
                type: "Sala de Cirurgia",
                capacity: 4,
                assignedDoctor: "Não atribuído",
                assignedPatient: "Não atribuído"
            },
            'OR-105': {
                id: 'OR-105',
                status: "Ocupada",
                type: "Sala de Cirurgia",
                capacity: 4,
                assignedDoctor: "Dr. Pereira",
                assignedPatient: "Ana Lima"
            }
        };
    }

    createOverlay() {
        const overlay = document.createElement('div');
        overlay.style.position = 'absolute';
        overlay.style.top = '20px';
        overlay.style.right = '20px';
        overlay.style.backgroundColor = 'rgba(0, 0, 0, 0.8)';
        overlay.style.color = 'white';
        overlay.style.padding = '20px';
        overlay.style.borderRadius = '5px';
        overlay.style.display = 'none';
        overlay.style.zIndex = '1000';
        document.body.appendChild(overlay);
        return overlay;
    }

    async displayRoomInfo(roomNumber) {
        const roomData = this.roomsData[roomNumber];
        if (!roomData) return;

        this.overlay.innerHTML = `
            <h3>Sala ${roomData.id}</h3>
            <p>Status: ${roomData.status}</p>
            <p>Tipo: ${roomData.type}</p>
            <p>Capacidade: ${roomData.capacity}</p>
            <p>Médico: ${roomData.assignedDoctor}</p>
            <p>Paciente: ${roomData.assignedPatient}</p>
        `;
    }

    toggleVisibility(roomNumber) {
        if (this.visible) {
            this.overlay.style.display = 'none';
            this.visible = false;
        } else {
            this.overlay.style.display = 'block';
            this.visible = true;
            this.displayRoomInfo(roomNumber);
        }
    }

    isInRoom(position) {
        // Definir as coordenadas das salas
        const rooms = {
            'OR-101': { minX: 5.5, maxX: 7.5, minZ: 2.0, maxZ: 4.0 },
            'OR-102': { minX: 2.5, maxX: 4.5, minZ: 2.0, maxZ: 4.0 },
            'OR-103': { minX: -0.5, maxX: 1.5, minZ: 2.0, maxZ: 4.0 },
            'OR-104': { minX: -3.5, maxX: -1.5, minZ: 2.0, maxZ: 4.0 },
            'OR-105': { minX: 4.5, maxX: 6.5, minZ: -4.0, maxZ: -2.0 }
        };

        for (const [roomNumber, bounds] of Object.entries(rooms)) {
            if (position.x >= bounds.minX && position.x <= bounds.maxX &&
                position.z >= bounds.minZ && position.z <= bounds.maxZ) {
                return roomNumber;
            }
        }
        return null;
    }
}

export default RoomInfo;