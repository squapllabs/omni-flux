import React from 'react';

interface FilterIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const FilterIcon: React.FC<FilterIconProps> = ({
  width = 18,
  height = 19,
  color = 'currentColor',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 18 19"
      style={style}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M17 2.6c0-.56 0-.84-.11-1.054a.998.998 0 0 0-.436-.437C16.24 1 15.96 1 15.4 1H2.6c-.56 0-.84 0-1.054.109a1 1 0 0 0-.437.437C1 1.76 1 2.04 1 2.6v.737c0 .245 0 .367.028.482a1 1 0 0 0 .12.29c.061.1.148.187.32.36l5.063 5.062c.173.173.26.26.321.36.055.09.096.188.12.29.028.114.028.235.028.474v4.756c0 .857 0 1.286.18 1.544a1 1 0 0 0 .674.416c.311.046.695-.145 1.461-.529l.8-.4c.322-.16.482-.24.599-.36a1 1 0 0 0 .231-.374c.055-.158.055-.338.055-.697v-4.348c0-.245 0-.367.028-.482a1 1 0 0 1 .12-.29c.06-.1.147-.186.317-.356l.004-.004 5.063-5.062c.172-.173.258-.26.32-.36a.994.994 0 0 0 .12-.29C17 3.706 17 3.584 17 3.345V2.6Z"
      />
    </svg>
  );
};

export default FilterIcon;
