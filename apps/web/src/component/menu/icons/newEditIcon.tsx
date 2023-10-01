import React, { SVGProps } from 'react';

interface StoreIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  height?: number | string;
  width?: number | string;
  onClick: () => void;
}

const NewEditIcon: React.FC<StoreIconProps> = ({
  color = 'currentColor',
  height = '20',
  width = '18',
  onClick,
  ...props
}) => (
  <div title="Edit">
    <svg
      width="40"
      height="40"
      viewBox="0 0 40 40"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      {...props}
    >
      <g clip-path="url(#clip0_117_57)">
        <path
          d="M24.1667 12.5C24.3856 12.2811 24.6455 12.1075 24.9314 11.9891C25.2174 11.8706 25.5239 11.8096 25.8334 11.8096C26.1429 11.8096 26.4494 11.8706 26.7354 11.9891C27.0214 12.1075 27.2812 12.2811 27.5001 12.5C27.719 12.7189 27.8926 12.9787 28.011 13.2647C28.1295 13.5506 28.1904 13.8571 28.1904 14.1667C28.1904 14.4762 28.1295 14.7827 28.011 15.0687C27.8926 15.3546 27.719 15.6145 27.5001 15.8333L16.2501 27.0833L11.6667 28.3333L12.9167 23.75L24.1667 12.5Z"
          stroke="#667085"
          stroke-width="1.66667"
          stroke-linecap="round"
          stroke-linejoin="round"
        />
      </g>
      <defs>
        <clipPath id="clip0_117_57">
          <rect
            width="20"
            height="20"
            fill="white"
            transform="translate(10 10)"
          />
        </clipPath>
      </defs>
    </svg>
  </div>
);

export default NewEditIcon;
