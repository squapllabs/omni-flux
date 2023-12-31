import React, { FC } from 'react';

interface menuIconProps {
    width?: number;
    height?: number;
    color?: string;
    onClick: () => void;
    style?: React.CSSProperties;
}

const MenuIcon: FC<menuIconProps> = ({
    width = 22,
    height = 27,
    color = '#475467',
    onClick,
    style,
}) => {
    return (
        <div style={style} >
            <svg xmlns="http://www.w3.org/2000/svg" width="44" height="44" viewBox="0 0 24 24"><path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 17h14M5 12h14M5 7h14"/></svg>        
        </div>
    );
};

export default MenuIcon;
