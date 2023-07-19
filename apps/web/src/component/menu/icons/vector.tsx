import React from 'react';

interface VectorProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const Vector: React.FC<VectorProps> = ({ width, height, color, style }) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill={color}
      viewBox="0 0 13 13"
      style={style}
    >
      <path
        fill="#000"
        d="M.93 12.793a.536.536 0 0 1 0-.757L11.122 1.845H3.33a.535.535 0 1 1 0-1.07h9.085a.536.536 0 0 1 .535.534v9.086a.535.535 0 0 1-1.07 0V2.602L1.69 12.792a.535.535 0 0 1-.758 0Z"
      />
    </svg>
  );
};

export default Vector;
