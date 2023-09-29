import React, { SVGProps } from 'react';

interface TickIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: number;
  width?: number;
}

const TickIcon: React.FC<TickIconProps> = ({
  color = 'black',
  height = 30,
  width = 24,
  ...props
}) => (
  <div title="Appprove">
    <svg
      xmlns="http://www.w3.org/2000/svg"
      height={height}
      width={width}
      viewBox="0 0 612 250"
      {...props}
    >
      <path
        d="M217.6 472.6c-6.2 6.2-16.4 6.2-22.6 0L56.5 337.2c-6.2-6.2-6.2-16.4 0-22.6l22.6-22.6c6.2-6.2 16.4-6.2 22.6 0l86.6 86.6L413.8 94.1c6.2-6.2 16.4-6.2 22.6 0l22.6 22.6c6.2 6.2 6.2 16.4 0 22.6L240.2 472.6c-6.2 6.2-16.4 6.2-22.6 0z"
        fill={color}
      />
    </svg>
  </div>
);

export default TickIcon;
