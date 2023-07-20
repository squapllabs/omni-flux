import React from 'react';

interface DownloadIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const DownloadIcon: React.FC<DownloadIconProps> = ({
  width = 20,
  height = 18,
  color = '#344054',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 20 18"
      style={style}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={1.667}
        d="M6.667 13.167 10 16.5m0 0 3.333-3.333M10 16.5V9m6.667 3.952a4.583 4.583 0 0 0-2.917-8.12.516.516 0 0 1-.445-.25 6.25 6.25 0 1 0-9.816 7.58"
      />
    </svg>
  );
};

export default DownloadIcon;
