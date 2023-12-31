import React, { FC, CSSProperties } from 'react';

interface AscendingIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const AscendingIcon: FC<AscendingIconProps> = ({
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
        d="M16.707 13.293a.999.999 0 0 0-1.414 0L13 15.586V8a1 1 0 1 0-2 0v7.586l-2.293-2.293a.999.999 0 1 0-1.414 1.414L12 19.414l4.707-4.707a.999.999 0 0 0 0-1.414z"
      />
    </svg>
  );
};

export default AscendingIcon;
