import React from 'react';
import Styles from '../../styles/customPopup.module.scss';
import CloseIcon from '../menu/icons/closeIcon';
interface DialogBoxProps {
    open: boolean;
    content: React.ReactNode;
    title: string;
    handleClose: () => void;
    width: string;
    description: string;
  }

  const CustomModelPopup: React.FC<DialogBoxProps> = ({
    open,
    content,
    title,
    description,
    handleClose,
    width = '50%',
  }) => {
    if(!open){
       return null;
    }
    const styles = {
        width: width,
    };
    return (
      <div className={Styles.dialog_mask}>
        <div className={Styles.dialog_box} style={styles}>
        <div className={Styles.mainContent}>
              <div className={Styles.popupHeader}>
                <div className={Styles.textContent_1}>
                  <h4>{title}</h4>
                  <span className={Styles.content}>{description}</span>
                </div>
  
                <button className={Styles.closeButton}>
                  <CloseIcon onClick={handleClose} />
                </button>
              </div>
            </div>
            <div className={Styles.selected}></div>
            <div className={Styles.popupContentContainer}>{content}</div>
        </div>
      </div>


    );




    // return (
    //         <div className={Styles.overlay}>
    //            <div className={Styles.popup} style={styles}>
    //            <h2>Here i am</h2>
    //             <a className={Styles.close} href="#">&times;</a>
    //             <div className={Styles.content} >
    //                 Thank to pop me out of that button, but now i'm done so you can close this window.
    //             </div>
    //            </div>
                
    //         </div>
    // )

  }

  export default CustomModelPopup;