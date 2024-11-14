import React, { useEffect } from 'react';

const Hospital3DView = () => {
  const containerStyle = {
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    width: '100%',
    height: '90vh',
    padding: '20px'
  };

  const frameStyle = {
    width: '1024px',
    height: '768px',
    border: 'none',
    backgroundColor: '#000'
  };

  return (
    <div style={containerStyle}>
      <iframe
        src="http://127.0.0.1:8080/Thumb_Raiser.html"
        style={frameStyle}
        title="Hospital 3D View"
        allow="fullscreen"
      />
    </div>
  );
};

export default Hospital3DView; 