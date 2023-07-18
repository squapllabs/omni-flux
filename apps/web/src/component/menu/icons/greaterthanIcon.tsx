import React, { SVGProps } from 'react';

interface GreaterIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: string | number;
  width?: string | number;
}

const GreaterIcon: React.FC<GreaterIconProps> = ({
  color = 'currentColor',
  height = '1em',
  width = '1em',
  ...props
}) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    height={height}
    width={width}
    viewBox="0 0 384 512"
    {...props}
  >
    <path
      d="M3.4 81.7c-7.9 15.8-1.5 35 14.3 42.9L280.5 256 17.7 387.4C1.9 395.3-4.5 414.5 3.4 430.3s27.1 22.2 42.9 14.3l320-160c10.8-5.4 17.7-16.5 17.7-28.6s-6.8-23.2-17.7-28.6l-320-160c-15.8-7.9-35-1.5-42.9 14.3z"
      fill={color}
    />
  </svg>
);

export default GreaterIcon;
