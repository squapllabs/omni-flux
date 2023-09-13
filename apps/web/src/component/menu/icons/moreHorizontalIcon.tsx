import React, { FC } from 'react';

interface MoreHorizIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
}

const MoreHorizIcon: FC<MoreHorizIconProps> = ({
  width = 24,
  height = 24,
  color = '#475467',
  onClick
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
      viewBox="0 0 24 24"
      onClick={onClick}
      style={iconStyle}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="1.5"
        d="M3 10c0-1.105 0.895-2 2-2s2 .895 2 2c0 1.105-0.895 2-2 2s-2-.895-2-2zm6 0c0-1.105 0.895-2 2-2s2 .895 2 2c0 1.105-0.895 2-2 2s-2-.895-2-2zm6 0c0-1.105 0.895-2 2-2s2 .895 2 2c0 1.105-0.895 2-2 2s-2-.895-2-2z"
      />
    </svg>
  );
};

export default MoreHorizIcon;
