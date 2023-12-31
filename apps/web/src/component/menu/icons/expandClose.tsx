import React, { SVGProps } from 'react';

interface ExpandCloseProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: string | number;
  width?: string | number;
  style?: any;
}

const ExpandClose: React.FC<ExpandCloseProps> = ({
  color,
  height = '1em',
  width = '1em',
  style = {},
  ...props
}) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={width}
    height={height}
    fill="currentColor"
    class="bi bi-chevron-up"
    viewBox="0 0 16 16"
  >
    {' '}
    <path
      fill-rule={color}
      d="M7.646 4.646a.5.5 0 0 1 .708 0l6 6a.5.5 0 0 1-.708.708L8 5.707l-5.646 5.647a.5.5 0 0 1-.708-.708l6-6z"
    />{' '}
  </svg>
);

export default ExpandClose;
