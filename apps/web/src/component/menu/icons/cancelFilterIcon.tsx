import React from 'react';

interface CancelFilterIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const CancelFilterIcon: React.FC<CancelFilterIconProps> = ({
  width = 24,
  height = 22,
  color = 'currentColor',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill={color}
      viewBox="0 0 24 22"
      style={style}
    >
      <path d="M0 .5h24v2.566l-9 9V21.5H9v-9.434l-9-9V.5zm22.5 1.934V2h-21v.434l9 9V20h3v-8.566l9-9zm-2.566 11l1.066 1.066-2.695 2.684 2.695 2.695-1.066 1.066-2.684-2.695-2.695 2.695-1.066-1.066 2.684-2.695-2.684-2.695 1.066-1.066 2.695 2.684 2.695-2.684Z" />
    </svg>
  );
};

export default CancelFilterIcon;
