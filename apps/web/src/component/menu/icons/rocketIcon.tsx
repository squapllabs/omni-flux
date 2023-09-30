import React, { FC } from 'react';

interface EditIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
  style?: React.CSSProperties;
}

const RocketIcon: FC<EditIconProps> = ({
  width = 22,
  height = 27,
  color = '#475467',
  onClick,
  style,
}) => {
  return (
    <div title="View">
     <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
<g clip-path="url(#clip0_32_53)">
    <path d="M11.1041 8.89604L4.5339 15.4662M5.24296 10.698L2.02479 9.93778C1.67073 9.85415 1.5462 9.41459 1.80269 9.15717L4.36198 6.59788C4.5339 6.42596 4.76716 6.32745 5.01156 6.32559L7.89704 6.3005M11.5315 2.9987C13.6791 4.46327 15.5368 6.32095 17.0014 8.46856M9.30121 14.7571L10.0614 17.9753C10.145 18.3294 10.5846 18.4539 10.842 18.1974L13.4013 15.6381C13.5732 15.4662 13.6717 15.2329 13.6736 14.9885L13.6987 12.1031M17.5525 6.95845L18.3033 3.34533C18.5078 2.36213 17.638 1.4923 16.6548 1.69675L13.0416 2.44762C11.9776 2.66879 11.0018 3.19571 10.2342 3.96424L7.26976 6.92778C6.13881 8.05874 5.4028 9.52517 5.17047 11.1078L5.16025 11.1756C5.01342 12.1858 5.35169 13.2052 6.07283 13.9273C6.79396 14.6484 7.81433 14.9867 8.82448 14.8389L8.89232 14.8287C10.4749 14.5973 11.9414 13.8604 13.0723 12.7294L16.0359 9.76586C16.8044 8.99826 17.3313 8.02249 17.5525 6.95845Z" stroke="#667085" stroke-width="1.66667" stroke-linecap="round" stroke-linejoin="round" />
</g>
<defs>
    <clipPath id="clip0_32_53">
        <rect width="20" height="20" fill="white" />
    </clipPath>
</defs>
</svg>
    </div>
  );
};

export default RocketIcon;
