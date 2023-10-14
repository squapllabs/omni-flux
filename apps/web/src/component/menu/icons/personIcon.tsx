import React, { SVGProps } from 'react';

interface PersonIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: string | number;
  width?: string | number;
}

const PersonIcon: React.FC<PersonIconProps> = ({
  color = 'currentColor',
  height = '1em',
  width = '1em',
  ...props
}) => (
  //   <svg
  //     xmlns="http://www.w3.org/2000/svg"
  //     height={height}
  //     width={width}
  //     viewBox="0 0 320 512"
  //     {...props}
  //   >
  //     <path
  //       d="M137.4 374.6c12.5 12.5 32.8 12.5 45.3 0l128-128c9.2-9.2 11.9-22.9 6.9-34.9s-16.6-19.8-29.6-19.8L32 192c-12.9 0-24.6 7.8-29.6 19.8s-2.2 25.7 6.9 34.9l128 128z"
  //       fill={color}
  //     />
  //   </svg>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    height={height}
    width={width}
    fill="currentColor"
    // class="bi bi-person"
    viewBox="0 0 16 16"
    {...props}
  >
    {' '}
    <path
      d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6zm2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0zm4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4zm-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10c-2.29 0-3.516.68-4.168 1.332-.678.678-.83 1.418-.832 1.664h10z"
      fill={color}
    />{' '}
  </svg>
);

export default PersonIcon;
