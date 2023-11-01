import React, { FC } from 'react';

interface OrderIconProps {
    width?: number;
    height?: number;
    color?: string;
    onClick: () => void;
    style?: React.CSSProperties;
}

const OrderIcon: FC<OrderIconProps> = ({
    width = 22,
    height = 27,
    color = '#475467',
    onClick,
    style,
}) => {
    return (
        <div style={style} >
            <svg xmlns="http://www.w3.org/2000/svg" width="43" height="43" viewBox="0 0 24 24"><path fill="currentColor" d="m17.371 19.827l2.84-2.796l-.626-.627l-2.214 2.183l-.956-.975l-.627.632l1.583 1.583ZM6.77 8.73h10.462v-1H6.769v1ZM18 22.115q-1.671 0-2.836-1.164T14 18.115q0-1.67 1.164-2.835T18 14.115q1.671 0 2.836 1.165T22 18.115q0 1.672-1.164 2.836Q19.67 22.115 18 22.115ZM4 20.77V5.615q0-.67.472-1.143Q4.944 4 5.615 4h12.77q.67 0 1.143.472q.472.472.472 1.143v5.945q-.244-.09-.485-.154q-.24-.064-.515-.1v-5.69q0-.231-.192-.424Q18.615 5 18.385 5H5.615q-.23 0-.423.192Q5 5.385 5 5.615V19.05h6.344q.068.41.176.802q.109.392.303.748l-.035.035l-1.134-.827l-1.346.961l-1.346-.961l-1.347.961l-1.346-.961L4 20.769Zm2.77-4.5h4.709q.056-.275.138-.515q.083-.24.193-.485H6.77v1Zm0-3.769h7.31q.49-.387 1.05-.645q.56-.259 1.197-.355H6.769v1ZM5 19.05V5v14.05Z"/></svg>
        </div>
    );
};

export default OrderIcon;
