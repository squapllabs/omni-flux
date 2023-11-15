import React from 'react';

interface AddIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const MailIcon: React.FC<AddIconProps> = ({
  width = 20,
  height = 20,
  color = 'white',
  style,
}) => {
  return (
    <svg
      width={width}
      height={height}
      style={style}
      xmlns="http://www.w3.org/2000/svg"
    >
      <path
        fill={color}
        d="M18.0833 3C18.0833 2.08334 17.3333 1.33334 16.4166 1.33334H3.08329C2.16663 1.33334 1.41663 2.08334 1.41663 3M18.0833 3V13C18.0833 13.9167 17.3333 14.6667 16.4166 14.6667H3.08329C2.16663 14.6667 1.41663 13.9167 1.41663 13V3M18.0833 3L9.74996 8.83334L1.41663 3"
        stroke="#667085"
        strokeWidth="1.66667"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </svg>
  );
};

export default MailIcon;
