import React, { FC, CSSProperties } from 'react';

interface DescendingIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const DescendingIcon: FC<DescendingIconProps> = ({
  width = 19,
  height = 16,
  color = '#000',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill={color}
      viewBox="0 0 19 16"
      style={style}
    >
      <path d="M9 7a.75.75 0 0 1-.75.75H1.5a.75.75 0 0 1 0-1.5h6.75A.75.75 0 0 1 9 7ZM1.5 1.75h12.75a.75.75 0 1 0 0-1.5H1.5a.75.75 0 0 0 0 1.5Zm5.25 10.5H1.5a.75.75 0 1 0 0 1.5h5.25a.75.75 0 1 0 0-1.5Zm11.78-2.03a.747.747 0 0 0-1.06 0L15 12.69V5.5a.75.75 0 1 0-1.5 0v7.19l-2.47-2.47a.75.75 0 1 0-1.06 1.06l3.75 3.75a.747.747 0 0 0 1.06 0l3.75-3.75a.747.747 0 0 0 0-1.06Z" />
    </svg>
  );
};

export default DescendingIcon;
