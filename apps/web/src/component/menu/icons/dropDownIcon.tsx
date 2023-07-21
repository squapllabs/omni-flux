import React, { FC, CSSProperties } from 'react';

interface DropdownIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const DropdownIcon: FC<DropdownIconProps> = ({
  width = 12,
  height = 8,
  color = '#475467',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 12 8"
      style={style}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="1.667"
        d="m1 1.5 5 5 5-5"
      />
    </svg>
  );
};

export default DropdownIcon;
