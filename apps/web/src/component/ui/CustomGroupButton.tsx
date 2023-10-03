import React from 'react';
import Button from './Button';

interface ButtonData {
  label: string;
  value: string;
}

interface GroupButtonProps {
  labels: ButtonData[];
  onClick: (value: string) => void;
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
      {labels.map(({ label, value }, index) => (
        <Button
          key={value}
          onClick={() => handleButtonClick(value)}
          style={{
            backgroundColor: activeButton === value ? '#D0D5DD' : 'white',
            color: activeButton === value ? 'black' : 'black',
            border: '1px solid #D0D5DD',
            padding: '10px 16px',
            fontSize: '0.75rem',
            borderRadius: index === 0 ? '8px 0 0 8px' : index === labels.length - 1 ? '0 8px 8px 0' : 'none', // Apply border radius based on index
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
