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
    <div title="View" onClick={onClick} style={style} role="button">
      <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M23.2049 11.745C22.3228 9.46324 20.7914 7.48996 18.8 6.06906C16.8086 4.64817 14.4445 3.84193 11.9999 3.75C9.55531 3.84193 7.19122 4.64817 5.19983 6.06906C3.20844 7.48996 1.67705 9.46324 0.794925 11.745C0.73535 11.9098 0.73535 12.0902 0.794925 12.255C1.67705 14.5368 3.20844 16.51 5.19983 17.9309C7.19122 19.3518 9.55531 20.1581 11.9999 20.25C14.4445 20.1581 16.8086 19.3518 18.8 17.9309C20.7914 16.51 22.3228 14.5368 23.2049 12.255C23.2645 12.0902 23.2645 11.9098 23.2049 11.745ZM11.9999 18.75C8.02493 18.75 3.82493 15.8025 2.30243 12C3.82493 8.1975 8.02493 5.25 11.9999 5.25C15.9749 5.25 20.1749 8.1975 21.6974 12C20.1749 15.8025 15.9749 18.75 11.9999 18.75Z" fill="#7F56D9" />
        <path d="M12 7.5C11.11 7.5 10.24 7.76392 9.49994 8.25839C8.75991 8.75285 8.18314 9.45566 7.84254 10.2779C7.50195 11.1002 7.41283 12.005 7.58647 12.8779C7.7601 13.7508 8.18869 14.5526 8.81802 15.182C9.44736 15.8113 10.2492 16.2399 11.1221 16.4135C11.995 16.5872 12.8998 16.4981 13.7221 16.1575C14.5443 15.8169 15.2471 15.2401 15.7416 14.5001C16.2361 13.76 16.5 12.89 16.5 12C16.5 10.8065 16.0259 9.66193 15.182 8.81802C14.3381 7.97411 13.1935 7.5 12 7.5ZM12 15C11.4067 15 10.8266 14.8241 10.3333 14.4944C9.83994 14.1648 9.45543 13.6962 9.22836 13.148C9.0013 12.5999 8.94189 11.9967 9.05765 11.4147C9.1734 10.8328 9.45912 10.2982 9.87868 9.87868C10.2982 9.45912 10.8328 9.1734 11.4147 9.05764C11.9967 8.94189 12.5999 9.0013 13.1481 9.22836C13.6962 9.45542 14.1648 9.83994 14.4944 10.3333C14.8241 10.8266 15 11.4067 15 12C15 12.7956 14.6839 13.5587 14.1213 14.1213C13.5587 14.6839 12.7957 15 12 15Z" fill="#7F56D9" />
      </svg>
    </div>
  );
};

export default RocketIcon;
