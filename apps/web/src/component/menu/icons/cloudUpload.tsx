import React, { FC } from 'react';

interface CloudUploadIconProps {
  width?: number;
  height?: number;
  color?: string;
}

const CloudUploadIcon: FC<CloudUploadIconProps> = ({
  width = 20,
  height = 20,
  color = '#475467',
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 47 33"
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="1.5"
        d="M32 32L24 24L16 32M24 24V42M40.7799 36.7809C42.7306 35.7175 44.2716 34.0347 45.1597 31.9982C46.0477 29.9617 46.2323 27.6874 45.6843 25.5343C45.1363 23.3812 43.8869 21.472 42.1333 20.1078C40.3796 18.7437 38.2216 18.0024 35.9999 18.0009H33.4799C32.8745 15.6594 31.7462 13.4856 30.1798 11.6429C28.6134 9.80025 26.6496 8.33665 24.4361 7.36216C22.2226 6.38767 19.817 5.92766 17.4002 6.01671C14.9833 6.10576 12.6181 6.74154 10.4823 7.87628C8.34649 9.01101 6.49574 10.6152 5.06916 12.5681C3.64259 14.5211 2.6773 16.772 2.24588 19.1517C1.81446 21.5315 1.92813 23.978 2.57835 26.3075C3.22856 28.6369 4.3984 30.7887 5.99992 32.6009M32 32L24 24L16 32"
        />
    </svg>
  );
};

export default CloudUploadIcon;
