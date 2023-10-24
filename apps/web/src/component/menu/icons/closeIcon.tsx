import React, { FC } from 'react';

interface CloseIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
  disabled?: boolean; // Add the disabled prop
}

const CloseIcon: FC<CloseIconProps> = ({
  width = 15,
  height = 15,
  color = '#475467',
  onClick,
  disabled = false, // Default value is false
}) => {
  const iconStyle: React.CSSProperties = {
    cursor: disabled ? 'not-allowed' : 'pointer', // Change cursor style based on 'disabled' prop
    pointerEvents: disabled ? 'none' : 'auto', // Disable pointer events based on 'disabled' prop
  };
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 18 18"
      onClick={disabled ? undefined : onClick} // Disable click event when 'disabled' is true
      style={iconStyle}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="1.667"
        d="M1.333 1.333l15.334 15.334M1.333 16.667L16.667 1.333"
      />
    </svg>
  );
};

export default CloseIcon;
