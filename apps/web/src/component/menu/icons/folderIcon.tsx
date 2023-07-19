import React from 'react';

interface FolderIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const FolderIcon: React.FC<FolderIconProps> = ({
  width,
  height,
  color,
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill={color}
      viewBox="0 0 20 24"
      style={style}
    >
      <path
        fill="#000"
        d="M12.333.333H3A2.34 2.34 0 0 0 .667 2.667v18.666A2.34 2.34 0 0 0 3 23.667h14a2.34 2.34 0 0 0 2.333-2.334v-14l-7-7Zm4.667 21H3V2.667h8.167V8.5H17v12.833ZM6.5 13.167v7H4.167v-7H6.5Zm7 2.333v4.667h2.333V15.5H13.5Zm-4.667-4.667v9.334h2.334v-9.334H8.833Z"
      />
    </svg>
  );
};

export default FolderIcon;
