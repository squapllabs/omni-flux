import React, { FC } from 'react';

interface EmailIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const EmailIcon: FC<EmailIconProps> = ({
  width = 20,
  height = 16,
  color = '#667085',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 20 16"
      style={style}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="1.667"
        d="M1.667 3.833 8.47 8.596c.55.386.826.579 1.126.654.264.066.541.066.806 0 .3-.075.575-.268 1.126-.654l6.804-4.763M5.667 14.668h8.666c1.4 0 2.1 0 2.635-.273a2.5 2.5 0 0 0 1.093-1.092c.272-.535.272-1.235.272-2.635V5.334c0-1.4 0-2.1-.272-2.635a2.5 2.5 0 0 0-1.093-1.093c-.535-.273-1.235-.273-2.635-.273H5.667c-1.4 0-2.1 0-2.635.273a2.5 2.5 0 0 0-1.093 1.093c-.272.534-.272 1.234-.272 2.635v5.333c0 1.4 0 2.1.272 2.635a2.5 2.5 0 0 0 1.093 1.092c.534.273 1.234.273 2.635.273Z"
      />
    </svg>
  );
};

export default EmailIcon;
