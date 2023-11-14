import React, { useEffect, useState } from 'react';
import Styles from '../../styles/expanses.module.scss';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import { useNavigate } from 'react-router-dom';
interface CancelFilterIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
  navigation: string;
  title: string;
  description: string;
  resetStore?: (e: string) => void;
}

const ProjectSubheader: React.FC<CancelFilterIconProps> = ({
  width = 24,
  height = 22,
  color = 'currentColor',
  style,
  title,
  navigation,
  description,
  resetStore,
}) => {
  const navigate = useNavigate();
  return (
    <div>
      <div className={Styles.sub_header}>
        <div
          className={Styles.logo}
          onClick={() => {
            navigate(`${navigation}`);
            resetStore('a');
          }}
        >
          <PreviousPageIcon width={15} height={15} color="#7f56d9" />
        </div>
        <div style={{ padding: '8px', display: 'flex' }}>
          <div className={Styles.vertical}>
            <div className={Styles.verticalLine}></div>
          </div>
        </div>
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            width: '700px',
          }}
        >
          <div className={Styles.textContent_1}>
            <h3>{title}</h3>
            <span className={Styles.content}>{description}</span>
          </div>
        </div>
      </div>
      <div className={Styles.dividerStyle}></div>
    </div>
  );
};

export default ProjectSubheader;
