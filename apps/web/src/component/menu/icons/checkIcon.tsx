import React, { FC, CSSProperties } from 'react';

interface CheckIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const CheckIcon: FC<CheckIconProps> = ({
  width = 22,
  height = 22,
  color = '#7F56D9',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 22 22"
      style={style}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="2"
        d="M21 10.086v.92a10 10 0 1 1-5.93-9.14M21 3 11 13.01l-3-3"
      />
    </svg>
  );
};

export default CheckIcon;
