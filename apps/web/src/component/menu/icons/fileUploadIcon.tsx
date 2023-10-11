import React, { FC, CSSProperties } from 'react';

interface FileUploadIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
  onClick: () => void;
}

const FileUploadIcon: FC<FileUploadIconProps> = ({
  width = 25,
  height = 25,
  color = '#475467',
  onClick,
  style,
}) => {
  return (
    <svg
      width={width}
      height={height}
      viewBox="0 0 18 20"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      onClick={onClick}
      cursor='pointer'
    >
      <path
        d="M10 18H3C2.73478 18 2.48043 17.8946 2.29289 17.7071C2.10536 17.5196 2 17.2652 2 17V3C2 2.73478 2.10536 2.48043 2.29289 2.29289C2.48043 2.10536 2.73478 2 3 2H8V5C8 5.79565 8.31607 6.55871 8.87868 7.12132C9.44129 7.68393 10.2044 8 11 8H14V10C14 10.2652 14.1054 10.5196 14.2929 10.7071C14.4804 10.8946 14.7348 11 15 11C15.2652 11 15.5196 10.8946 15.7071 10.7071C15.8946 10.5196 16 10.2652 16 10V6.94C15.9896 6.84813 15.9695 6.75763 15.94 6.67V6.58C15.8919 6.47718 15.8278 6.38267 15.75 6.3L9.75 0.3C9.66734 0.222216 9.57282 0.158081 9.47 0.11C9.44015 0.10576 9.40985 0.10576 9.38 0.11C9.27841 0.0517412 9.16622 0.0143442 9.05 0H3C2.20435 0 1.44129 0.316071 0.87868 0.87868C0.316071 1.44129 0 2.20435 0 3V17C0 17.7956 0.316071 18.5587 0.87868 19.1213C1.44129 19.6839 2.20435 20 3 20H10C10.2652 20 10.5196 19.8946 10.7071 19.7071C10.8946 19.5196 11 19.2652 11 19C11 18.7348 10.8946 18.4804 10.7071 18.2929C10.5196 18.1054 10.2652 18 10 18ZM10 3.41L12.59 6H11C10.7348 6 10.4804 5.89464 10.2929 5.70711C10.1054 5.51957 10 5.26522 10 5V3.41ZM5 6C4.73478 6 4.48043 6.10536 4.29289 6.29289C4.10536 6.48043 4 6.73478 4 7C4 7.26522 4.10536 7.51957 4.29289 7.70711C4.48043 7.89464 4.73478 8 5 8H6C6.26522 8 6.51957 7.89464 6.70711 7.70711C6.89464 7.51957 7 7.26522 7 7C7 6.73478 6.89464 6.48043 6.70711 6.29289C6.51957 6.10536 6.26522 6 6 6H5ZM11 10H5C4.73478 10 4.48043 10.1054 4.29289 10.2929C4.10536 10.4804 4 10.7348 4 11C4 11.2652 4.10536 11.5196 4.29289 11.7071C4.48043 11.8946 4.73478 12 5 12H11C11.2652 12 11.5196 11.8946 11.7071 11.7071C11.8946 11.5196 12 11.2652 12 11C12 10.7348 11.8946 10.4804 11.7071 10.2929C11.5196 10.1054 11.2652 10 11 10ZM17.71 15.29L15.71 13.29C15.6149 13.199 15.5028 13.1276 15.38 13.08C15.1365 12.98 14.8635 12.98 14.62 13.08C14.4972 13.1276 14.3851 13.199 14.29 13.29L12.29 15.29C12.1017 15.4783 11.9959 15.7337 11.9959 16C11.9959 16.2663 12.1017 16.5217 12.29 16.71C12.4783 16.8983 12.7337 17.0041 13 17.0041C13.2663 17.0041 13.5217 16.8983 13.71 16.71L14 16.41V19C14 19.2652 14.1054 19.5196 14.2929 19.7071C14.4804 19.8946 14.7348 20 15 20C15.2652 20 15.5196 19.8946 15.7071 19.7071C15.8946 19.5196 16 19.2652 16 19V16.41L16.29 16.71C16.383 16.8037 16.4936 16.8781 16.6154 16.9289C16.7373 16.9797 16.868 17.0058 17 17.0058C17.132 17.0058 17.2627 16.9797 17.3846 16.9289C17.5064 16.8781 17.617 16.8037 17.71 16.71C17.8037 16.617 17.8781 16.5064 17.9289 16.3846C17.9797 16.2627 18.0058 16.132 18.0058 16C18.0058 15.868 17.9797 15.7373 17.9289 15.6154C17.8781 15.4936 17.8037 15.383 17.71 15.29ZM9 16C9.26522 16 9.51957 15.8946 9.70711 15.7071C9.89464 15.5196 10 15.2652 10 15C10 14.7348 9.89464 14.4804 9.70711 14.2929C9.51957 14.1054 9.26522 14 9 14H5C4.73478 14 4.48043 14.1054 4.29289 14.2929C4.10536 14.4804 4 14.7348 4 15C4 15.2652 4.10536 15.5196 4.29289 15.7071C4.48043 15.8946 4.73478 16 5 16H9Z"
        fill="black"
      />
    </svg>
  );
};

export default FileUploadIcon;
