import React from 'react';

interface ViewIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
  onClick: () => void;
}

const ViewIcon: React.FC<ViewIconProps> = ({
    width = 24,
    height = 24,
    onClick,
    color = 'currentColor',
    style,
  }) => {
    return (
        <svg xmlns="http://www.w3.org/2000/svg" 
        viewBox="0 0 20 20"
        width={width}
        height={height}
        onClick={onClick}
        >
            <path d="M.2 10a11 11 0 0 1 19.6 0A11 11 0 0 1 .2 10zm9.8 4a4 4 0 1 0 0-8 4 4 0 0 0 0 8zm0-2a2 2 0 1 1 0-4 2 2 0 0 1 0 4z"/>
        </svg>
      );
  };

  export default ViewIcon;