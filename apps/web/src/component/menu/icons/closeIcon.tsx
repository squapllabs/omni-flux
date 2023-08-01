import React, { FC } from 'react';

interface CloseIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
}

const CloseIcon: FC<CloseIconProps> = ({
  width = 15,
  height = 15,
  color = '#475467',
  onClick,
}) => {
  const iconStyle: React.CSSProperties = {
    cursor: 'pointer',
  };
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 18 18"
      onClick={onClick}
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
