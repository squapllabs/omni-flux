import React, { FC, CSSProperties } from 'react';

interface AttachmentIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const AttachmentIcon: FC<AttachmentIconProps> = ({
  width = 25,
  height = 25,
  color = '#475467',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 24 24"
      width={width}
      height={height}
    >
      <g>
        <path d="M0 0h24v24H0z" fill="none" />
        <path
          d="M14.828 7.757l-5.656 5.657a1 1 0 1 0 1.414 1.414l5.657-5.656A3 3 0 1 0 12 4.929l-5.657 5.657a5 5 0 1 0 7.071 7.07L19.071 12l1.414 1.414-5.657 5.657a7 7 0 1 1-9.9-9.9l5.658-5.656a5 5 0 0 1 7.07 7.07L12 16.244A3 3 0 1 1 7.757 12l5.657-5.657 1.414 1.414z"
          fill={color}
        />
      </g>
    </svg>
  );
};

export default AttachmentIcon;
