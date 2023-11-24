
import React, { useState } from 'react';

interface CustomChipWithDeleteProps {
  label: string;
  onDelete: () => void;
  variant?: 'default' | 'outlined'; 
}

const CustomChipWithDelete: React.FC<CustomChipWithDeleteProps> = ({
  label,
  onDelete,
  variant = 'default',
}) => {
  const [isVisible, setIsVisible] = useState(true);

  const handleDelete = () => {
    setIsVisible(false);
    onDelete();
  };

  if (!isVisible) return null;

  const containerStyle = {
    display: 'flex',
    alignItems: 'center',
    padding: '4px 8px',
    margin: '4px',
    backgroundColor: variant ==='default' ?  '#E0E0E0' : 'none',
    borderRadius: '16px', 
    border: variant === 'outlined' ? '1.5px solid #E0E0E0' : 'none',
  };

  const buttonStyle = {
    background: 'none',
    border: 'none',
    cursor: 'pointer',
  };

  return (
    <div style={containerStyle}>
      <span style={{ flex: 1 }}>{label}</span>
      <button onClick={handleDelete} style={buttonStyle}>
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="16"
          height="16"
          fill="currentColor"
          className="bi bi-x"
          viewBox="0 0 13 13"
        >
          <path
            fillRule="evenodd"
            d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 1 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"
          />
        </svg>
      </button>
    </div>
  );
};

export default CustomChipWithDelete;
