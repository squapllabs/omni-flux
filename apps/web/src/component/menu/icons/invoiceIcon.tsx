import React from 'react';

interface AddIconProps {
    width?: number;
    height?: number;
    color?: string;
    style?: React.CSSProperties;
    onClick: () => void;
}

const InvoiceIcon: React.FC<AddIconProps> = ({
    height = '1.2em',
    width = '1.2em',
    color = '#7f56d',
    onClick,
    style,
}) => {
    return (

        <svg xmlns="http://www.w3.org/2000/svg" height={height}
            width={width} viewBox="0 0 24 24"><path fill={color} d="m18 2l1.5 1.5L21 2v7.13l-2 2V4.91H5v14.18h5.5v1.41L9 22l-1.5-1.5L6 22l-1.5-1.5L3 22V2l1.5 1.5L6 2l1.5 1.5L9 2l1.5 1.5L12 2l1.5 1.5L15 2l1.5 1.5L18 2m-5 17.96l6.13-6.13l2.04 2.04L15.04 22H13v-2.04m6.83-6.83l.98-.98l.02-.02c.05-.04.11-.08.17-.1a.5.5 0 0 1 .53.12l1.32 1.32c.2.2.2.53 0 .72l-.98.98l-2.04-2.04m-1.83-1l-.87.87H6v-2h12v1.13M15.13 15l-2 2H6v-2h9.13M18 9V7H6v2h12Z" /></svg>
    );
};

export default InvoiceIcon;
