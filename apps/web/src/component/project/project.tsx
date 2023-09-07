import React, { useState } from 'react';
import CustomGroupButton from '../ui/CustomGroupButton';
import Button from '../ui/Button';
import Styles from '../../styles/project.module.scss';
import ProjectGeneralDetails from './projectComponent/projectGeneralDetails';
import ProjectBomConfig from './projectComponent/projectBomConfig';
import ProjectSiteConfig from './projectComponent/projectSiteConfig';

const Project = () => {
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'General settings', value: 'PGS' },
    { label: 'Site Configuration', value: 'PSC' },
    { label: 'BOM configuration', value: 'PBC' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('PGS');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  return (
    <div className={Styles.Container}>
      <div className={Styles.groupButton}>
        <CustomGroupButton
          labels={buttonLabels}
          //   onClick={handleGroupButtonClick}
          activeButton={activeButton}
        />
      </div>
      <div className={Styles.mainBody}>
        {activeButton === 'PGS' ? (
          <ProjectGeneralDetails setActiveButton={setActiveButton} />
        ) : (
          ''
        )}
        {activeButton === 'PBC' ? (
          <ProjectBomConfig setActiveButton={setActiveButton} />
        ) : (
          ''
        )}
        {activeButton === 'PSC' ? (
          <ProjectSiteConfig setActiveButton={setActiveButton} />
        ) : (
          ''
        )}
      </div>
    </div>
  );
};

export default Project;
