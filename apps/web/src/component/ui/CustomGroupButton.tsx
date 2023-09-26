import React, { CSSProperties } from 'react';
import Button from './Button';

interface ButtonData {
  label: string;
  value: string;
}

interface GroupButtonProps {
  labels: ButtonData[];
  onClick: (value: string) => void; // Update the onClick callback to receive the value
  activeButton?: string | null;
}

const CustomGroupButton: React.FC<GroupButtonProps> = ({
  labels,
  onClick,
  activeButton,
}) => {
  const handleButtonClick = (value: string) => {
    onClick(value);
  };

  return (
    <div style={{ display: 'flex' }}>
      {labels.map(({ label, value }) => (
        <Button
          key={value}
          onClick={() => handleButtonClick(value)}
          style={{
            backgroundColor: activeButton === value ? '#D0D5DD' : 'white',
            color: activeButton === value ? 'Black' : 'black',
            border: '1px solid #D0D5DD',
            padding: '10px, 16px, 10px, 16px',
            fontSize:'0.75rem'
          }}
          justify="center"
          size="small"
        >
          {label}
        </Button>
      ))}
    </div>
  );
};

export default CustomGroupButton;
