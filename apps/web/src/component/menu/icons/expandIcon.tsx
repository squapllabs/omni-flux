import React, { SVGProps } from 'react';

interface ExpandIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: string | number;
  width?: string | number;
  style? : any ;
}

const ExpandIcon: React.FC<ExpandIconProps> = ({
  color,
  height = '1em',
  width = '1em',
  style = {},
  ...props
}) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    height={height}
    width={width}
    viewBox="0 0 512 512"
    {...props}
  >
    <path d="M233.4 406.6c12.5 12.5 32.8 12.5 45.3 0l192-192c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L256 338.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l192 192z"
    fill={color}
    />
  </svg>
);

export default ExpandIcon;
