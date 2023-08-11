import React from 'react';
import Button from '../ui/Button';
import Styles from '../../styles/popupexpanses.module.scss';

const PopupExpanse = () => {
  return (
    <div className={Styles.container}>
      <div className={Styles.button}>
        <Button color="primary" shape="rectangle" justify="center" size="small">
          Download
        </Button>
        <Button color="primary" shape="rectangle" justify="center" size="small">
          Bulk Upload
        </Button>
      </div>
    </div>
  );
};

export default PopupExpanse;
