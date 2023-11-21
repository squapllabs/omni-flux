import React, { FC, CSSProperties } from 'react';

interface WarningIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const WarningIcon: FC<WarningIconProps> = ({
  width = 25,
  height = 25,
  color = '#7F56D9',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 30 30"
      style={style}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="1.5"
        d="M15 20a1 1 0 1 1-2 0a1 1 0 0 1 2 0Zm-1.75-3.25a.75.75 0 0 0 1.5 0v-6.5a.75.75 0 0 0-1.5 0v6.5ZM11.592 4.17c1.046-1.894 3.77-1.895 4.816 0l9.25 16.75c1.012 1.833-.314 4.08-2.407 4.08H4.757c-2.093 0-3.42-2.246-2.408-4.079l9.243-16.75Zm3.502.725a1.25 1.25 0 0 0-2.188 0L3.662 21.646A1.25 1.25 0 0 0 4.757 23.5H23.25a1.25 1.25 0 0 0 1.094-1.854l-9.25-16.751Z"
        // d="M21 10.086v.92a10 10 0 1 1-5.93-9.14M21 3 11 13.01l-3-3"
      />
    </svg>
  );
};

export default WarningIcon;
