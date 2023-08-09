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
        d="M40.7799 31.7809C42.7306 30.7175 44.2716 29.0347 45.1597 26.9982C46.0477 24.9617 46.2323 22.6874 45.6843 20.5343C45.1363 18.3812 43.8869 16.472 42.1333 15.1078C40.3796 13.7437 38.2216 13.0024 35.9999 13.0009H33.4799C32.8745 10.6594 31.7462 8.48561 30.1798 6.64293C28.6134 4.80025 26.6496 3.33665 24.4361 2.36216C22.2226 1.38767 19.817 0.927662 17.4002 1.01671C14.9833 1.10576 12.6181 1.74154 10.4823 2.87628C8.34649 4.01101 6.49574 5.61515 5.06916 7.56811C3.64259 9.52107 2.6773 11.772 2.24588 14.1517C1.81446 16.5315 1.92813 18.978 2.57835 21.3075C3.22856 23.6369 4.3984 25.7887 5.99992 27.6009"
        // d="M16 17a4 4 0 11-8 0M12 2v10m0 0v10m0-10l-3-3m3 3l3-3m5 12H6a4 4 0 00-4 4v2a4 4 0 004 4h12a4 4 0 004-4v-2a4 4 0 00-4-4z"
      />
    </svg>
  );
};

export default CloudUploadIcon;
