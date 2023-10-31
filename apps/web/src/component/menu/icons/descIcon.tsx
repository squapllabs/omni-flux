import React, { FC, CSSProperties } from 'react';

interface DescIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const DescIcon: FC<DescIconProps> = ({
  width = 19,
  height = 16,
  color = '#000',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="24"
      height="24"
      viewBox="0 0 24 24"
    >
      <path
        fill="currentColor"
        d="m13 5.586l-4.707 4.707a.999.999 0 1 0 1.414 1.414L12 9.414V17a1 1 0 1 0 2 0V9.414l2.293 2.293a.997.997 0 0 0 1.414 0a.999.999 0 0 0 0-1.414L13 5.586z"
      />
    </svg>
  );
};

export default DescIcon;
