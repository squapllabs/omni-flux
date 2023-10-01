import React, { SVGProps } from 'react';

interface DashboardIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: string | number;
  width?: string | number;
}

const DashboardIcon: React.FC<DashboardIconProps> = ({
  color = 'currentColor',
  height = '1em',
  width = '1em',
  ...props
}) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    height={height}
    width={width}
    fill="currentColor"
    class="bi bi-columns-gap"
    viewBox="0 0 16 16"
  >
    {' '}
    <path
      d="M6 1v3H1V1h5zM1 0a1 1 0 0 0-1 1v3a1 1 0 0 0 1 1h5a1 1 0 0 0 1-1V1a1 1 0 0 0-1-1H1zm14 12v3h-5v-3h5zm-5-1a1 1 0 0 0-1 1v3a1 1 0 0 0 1 1h5a1 1 0 0 0 1-1v-3a1 1 0 0 0-1-1h-5zM6 8v7H1V8h5zM1 7a1 1 0 0 0-1 1v7a1 1 0 0 0 1 1h5a1 1 0 0 0 1-1V8a1 1 0 0 0-1-1H1zm14-6v7h-5V1h5zm-5-1a1 1 0 0 0-1 1v7a1 1 0 0 0 1 1h5a1 1 0 0 0 1-1V1a1 1 0 0 0-1-1h-5z"
      fill={color}
    />{' '}
  </svg>
);

export default DashboardIcon;
