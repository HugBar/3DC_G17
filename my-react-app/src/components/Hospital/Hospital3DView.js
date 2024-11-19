import React, { } from 'react';

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
        src="WebsiteHospitalThreeJS-main/Hospital_3D_module.html"
        style={frameStyle}
        title="Hospital 3D View"
        allow="fullscreen"
      />
    </div>
  );
};

export default Hospital3DView; 