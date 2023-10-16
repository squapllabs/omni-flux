import React, { FC } from 'react';

interface EditIconProps {
    width?: number;
    height?: number;
    color?: string;
    // onClick: () => void;
    style?: React.CSSProperties;
}

const DocumentIcon: FC<EditIconProps> = ({
    width = 30,
    height = 28,
    color = '#475467',
    // onClick,
    style,
}) => {
    return (
        <div  style={style} role="button">
            <svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" viewBox="0 0 32 32"><path fill="currentColor" d="m25.7 9.3l-7-7c-.2-.2-.4-.3-.7-.3H8c-1.1 0-2 .9-2 2v24c0 1.1.9 2 2 2h16c1.1 0 2-.9 2-2V10c0-.3-.1-.5-.3-.7zM18 4.4l5.6 5.6H18V4.4zM24 28H8V4h8v6c0 1.1.9 2 2 2h6v16z" /><path fill="currentColor" d="M10 22h12v2H10zm0-6h12v2H10z" /></svg>
        </div>
    );
};

export default DocumentIcon;
