import React from 'react';
import CloseIcon from '../menu/icons/closeIcon';

const Popup = (props) => {
    const { title, children, openPopup, setOpenPopup,content } = props;

    const handleClose = () => {
        setOpenPopup(false);
    };

    return (
        <div style={{ display: openPopup ? 'block' : 'none' }}>
            <div
                style={{
                    position: 'fixed',
                    top: '0',
                    left: '0',
                    width: '100%',
                    height: '100%',
                    background: 'rgba(0, 0, 0, 0.5)',
                    display: 'flex',
                    justifyContent: 'center',
                    alignItems: 'center',
                }}
            >
                <div
                    style={{
                        background: '#fff',
                        paddingTop: '45px',
                        paddingLeft: '25px',
                        paddingRight: '25px',
                        height: '100vh'
                    }}
                >
                    <div style={{ display: 'flex', justifyContent: 'space-between', paddingBottom: '10px'}}>
                        <h2>{title}</h2>
                        <div onClick={handleClose}>
                            <CloseIcon />
                        </div>
                    </div>
                    <div>{children}</div>
                    <div style={{display: 'flex', justifyContent: 'center'}}>{content}</div>
                </div>
            </div>
        </div>
    );
};

export default Popup;
