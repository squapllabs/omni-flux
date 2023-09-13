import React, { FC } from 'react';

interface MoreVertIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
}

const MoreVertIcon: FC<MoreVertIconProps> = ({
  width = 20,
  height = 20,
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
        d="M12 5c1.105 0 2 .895 2 2s-.895 2-2 2-2-.895-2-2 .895-2 2-2zM12 12c1.105 0 2 .895 2 2s-.895 2-2 2-2-.895-2-2 .895-2 2-2zM12 19c1.105 0 2 .895 2 2s-.895 2-2 2-2-.895-2-2 .895-2 2-2z"
      />
    </svg>
  );
};

export default MoreVertIcon;
