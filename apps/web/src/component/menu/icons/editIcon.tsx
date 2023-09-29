import React, { FC } from 'react';

interface EditIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
  style?: React.CSSProperties;
}

const EditIcon: FC<EditIconProps> = ({
  width = 22,
  height = 27,
  color = '#475467',
  onClick,
  style,
}) => {
  return (
    <div title="Edit">
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width={width}
        height={height}
        style={style}
        fill="none"
        viewBox="0 0 18 20"
        onClick={onClick}
      >
        <path
          stroke={color}
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth="1.667"
          d="M4.667 12l6-6m-3 9l9-9-3-3-9 9v3h3z"
        />
      </svg>
    </div>
  );
};

export default EditIcon;
