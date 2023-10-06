import React, { FC } from 'react';

interface MoneyIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const MoneyIcon: FC<MoneyIconProps> = ({
  width = 28,
  height = 28,
  color = '#475467',
  style,
}) => {
  return (
    <div>
      <svg
        width={width}
        height={height}
        viewBox="0 0 28 30"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
      >
        <path
          d="M14.0007 16.6647C12.763 16.6647 11.576 17.1563 10.7008 18.0315C9.82565 18.9067 9.33398 20.0937 9.33398 21.3313C9.33398 22.569 9.82565 23.756 10.7008 24.6312C11.576 25.5063 12.763 25.998 14.0007 25.998C15.2383 25.998 16.4253 25.5063 17.3005 24.6312C18.1757 23.756 18.6673 22.569 18.6673 21.3313C18.6673 20.0937 18.1757 18.9067 17.3005 18.0315C16.4253 17.1563 15.2383 16.6647 14.0007 16.6647ZM12.0007 21.3313C12.0007 20.8009 12.2114 20.2922 12.5864 19.9171C12.9615 19.5421 13.4702 19.3313 14.0007 19.3313C14.5311 19.3313 15.0398 19.5421 15.4149 19.9171C15.7899 20.2922 16.0007 20.8009 16.0007 21.3313C16.0007 21.8618 15.7899 22.3705 15.4149 22.7456C15.0398 23.1206 14.5311 23.3313 14.0007 23.3313C13.4702 23.3313 12.9615 23.1206 12.5864 22.7456C12.2114 22.3705 12.0007 21.8618 12.0007 21.3313Z"
          fill="black"
        />
        <path
          d="M21.368 6.81931L17.1293 0.876648L1.544 13.3273L0.68 13.318V13.3313H0V29.3313H28V13.3313H26.7173L24.1653 5.86598L21.368 6.81931ZM23.9 13.3313H10.5293L20.488 9.93665L22.5173 9.28732L23.9 13.3313ZM18.7333 7.71798L8.45333 11.222L16.5947 4.71798L18.7333 7.71798ZM2.66667 24.2233V18.4366C3.22919 18.2373 3.74014 17.9149 4.16226 17.4931C4.58438 17.0712 4.90702 16.5604 5.10667 15.998H22.8933C23.0929 16.5606 23.4155 17.0716 23.8376 17.4937C24.2597 17.9158 24.7707 18.2384 25.3333 18.438V24.2246C24.7707 24.4242 24.2597 24.7468 23.8376 25.1689C23.4155 25.591 23.0929 26.102 22.8933 26.6646H5.10933C4.90958 26.1016 4.58662 25.5902 4.16403 25.1678C3.74143 24.7454 3.22987 24.4228 2.66667 24.2233Z"
          fill="black"
        />
      </svg>
    </div>
  );
};

export default MoneyIcon;
