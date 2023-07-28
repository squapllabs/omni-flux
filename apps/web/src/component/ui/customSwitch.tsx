import React from 'react';

interface ModeChangeProps {
  isActive: boolean;
  onClick: () => void;
}

const ModeChange: React.FC<ModeChangeProps> = ({ isActive, onClick }) => {
  return (
    <div
      className={`custom-switch ${isActive ? 'active' : 'inactive'}`}
      onClick={onClick}
    >
      <div className="slider"></div>
      <span className="label">{isActive ? 'ACTIVE' : 'INACTIVE'}</span>
    </div>
  );
};

export default ModeChange;
