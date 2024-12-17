class RoomInfo {
    constructor() {
        this.visible = false;
        this.currentRoom = null;
        this.overlay = this.createOverlay();
        this.roomsData = {};
        this.loadRoomsData();
    }

    async loadRoomsData() {
        try {
            const rooms = ['OR-101', 'OR-102', 'OR-103', 'OR-104', 'OR-105'];
            for (const roomId of rooms) {
                const response = await fetch(`https://localhost:5001/api/surgery-room/${roomId}`);
                if (response.ok) {
                    const data = await response.json();
                    this.roomsData[roomId] = data;
                }
            }
        } catch (error) {
            console.error("Error fetching room data:", error);
        }
    }

    createOverlay() {
        const overlay = document.createElement('div');
        overlay.style.position = 'absolute';
        overlay.style.top = '20px';
        overlay.style.right = '20px';
        overlay.style.backgroundColor = 'rgba(25, 25, 25, 0.95)';
        overlay.style.color = '#ffffff';
        overlay.style.padding = '25px';
        overlay.style.borderRadius = '10px';
        overlay.style.display = 'none';
        overlay.style.zIndex = '1000';
        overlay.style.boxShadow = '0 4px 6px rgba(0, 0, 0, 0.3)';
        overlay.style.minWidth = '300px';
        overlay.style.fontFamily = 'Arial, sans-serif';
        overlay.style.border = '1px solid rgba(255, 255, 255, 0.1)';
        document.body.appendChild(overlay);
        return overlay;
    }

    async displayRoomInfo(roomNumber) {
        const roomData = this.roomsData[roomNumber];
        if (!roomData) return;

        const getStatusColor = (status) => {
            switch(status.toLowerCase()) {
                case 'available': return '#4CAF50'; // Green
                case 'occupied': return '#f44336';  // Red
                case 'maintenance': return '#ff9800'; // Yellow
                default: return '#ffffff';
            }
        };

        const maintenanceInfo = roomData.maintenanceSlots.length > 0 
            ? roomData.maintenanceSlots.map(slot => 
                `<div class="maintenance-slot">
                    <span>📅 ${new Date(slot.startTime).toLocaleDateString()}</span>
                    <span>${new Date(slot.startTime).toLocaleTimeString()} - ${new Date(slot.endTime).toLocaleTimeString()}</span>
                 </div>`
              ).join('')
            : '<p class="no-maintenance">No scheduled maintenance</p>';

        this.overlay.innerHTML = `
            <style>
                .room-header { 
                    font-size: 24px;
                    margin-bottom: 20px;
                    border-bottom: 1px solid rgba(255,255,255,0.1);
                    padding-bottom: 10px;
                }
                .room-info-row {
                    display: flex;
                    justify-content: space-between;
                    margin: 10px 0;
                    align-items: center;
                }
                .status-badge {
                    padding: 5px 10px;
                    border-radius: 15px;
                    font-weight: bold;
                }
                .maintenance-slot {
                    background: rgba(255,255,255,0.1);
                    padding: 10px;
                    margin: 5px 0;
                    border-radius: 5px;
                    display: flex;
                    justify-content: space-between;
                }
                .section-title {
                    font-size: 16px;
                    margin: 15px 0 10px 0;
                    color: #aaa;
                }
                .no-maintenance {
                    color: #888;
                    font-style: italic;
                }
            </style>
            <div class="room-header">Room ${roomData.id}</div>
            <div class="room-info-row">
                <span>Status:</span>
                <span class="status-badge" style="background-color: ${getStatusColor(roomData.status)}">
                    ${roomData.status}
                </span>
            </div>
            <div class="room-info-row">
                <span>Type:</span>
                <span>${roomData.type}</span>
            </div>
            <div class="room-info-row">
                <span>Capacity:</span>
                <span>${roomData.capacity} people</span>
            </div>
            <div class="section-title">Equipment</div>
            <div style="color: #ddd">
                ${roomData.assignedEquipment.length ? roomData.assignedEquipment.join(', ') : 'No equipment assigned'}
            </div>
            <div class="section-title">Maintenance Schedule</div>
            ${maintenanceInfo}
        `;
    }

    toggleVisibility(roomNumber) {
        if (this.visible && this.currentRoom === roomNumber) {
            // Se já estiver mostrando info desta sala, esconde
            this.overlay.style.display = 'none';
            this.visible = false;
            this.currentRoom = null;
        } else {
            // Se estiver escondido ou mostrando outra sala, mostra esta
            this.overlay.style.display = 'block';
            this.visible = true;
            this.currentRoom = roomNumber;
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

    getRoomFromPosition(position) {
        // Define camera positions that correspond to each room view
        const roomViews = {
            'OR-101': { x: 6.50, y: 4, z: 3.15 },
            'OR-102': { x: 3.50, y: 4, z: 3.15 },
            'OR-103': { x: 0.50, y: 4, z: 3.15 },
            'OR-104': { x: -2.50, y: 4, z: 3.15 },
            'OR-105': { x: 5.50, y: 4, z: -3.15 }
        };

        // Find which room view matches the camera position
        for (const [roomNumber, view] of Object.entries(roomViews)) {
            if (Math.abs(position.x - view.x) < 0.1 && 
                Math.abs(position.y - view.y) < 0.1 && 
                Math.abs(position.z - view.z) < 0.1) {
                return roomNumber;
            }
        }
        return null;
    }
}

export default RoomInfo;