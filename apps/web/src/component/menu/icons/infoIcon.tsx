import React, { SVGProps } from 'react';

interface InfoIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: number;
  width?: number;
}

const InfoIcon: React.FC<InfoIconProps> = ({
  height = 45,
  width = 25,
  color = '#7F56D9',
  ...props
}) => (
  <div title="Info">
    <svg
      xmlns="http://www.w3.org/2000/svg"
      height={height}
      width={width}
      viewBox=" 0 0 125 128"
      {...props}
    >
      <path 
      stroke={color}
      d="M 64 6 C 32 6 6 32 6 64 C 6 96 32 122 64 122 C 96 122 122 96 122 64 C 122 32 96 6 64 6 z M 64 12 C 92.7 12 116 35.3 116 64 C 116 92.7 92.7 116 64 116 C 35.3 116 12 92.7 12 64 C 12 35.3 35.3 12 64 12 z M 64 30 A 9 9 0 0 0 64 48 A 9 9 0 0 0 64 30 z M 64 59 C 59 59 55 63 55 68 L 55 92 C 55 97 59 101 64 101 C 69 101 73 97 73 92 L 73 68 C 73 63 69 59 64 59 z"></path>
    </svg>
  </div>
);

export default InfoIcon;
