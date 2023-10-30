import React from 'react';

interface AddIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const AddIcon: React.FC<AddIconProps> = ({
  width = 24,
  height = 24,
  color = '#7f56d',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 24 24"
      width={width}
      height={height}
      style={style}
    >
      <path
        d="M19 11H13V5c0-.55-.45-1-1-1s-1 .45-1 1v6H5c-.55 0-1 .45-1 1s.45 1 1 1h6v6c0 .55.45 1 1 1s1-.45 1-1v-6h6c.55 0 1-.45 1-1s-.45-1-1-1z"
        fill={color}
      />
    </svg>
  );
};

export default AddIcon;
