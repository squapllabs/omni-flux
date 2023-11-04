import React, { useState } from 'react';
import CustomTabComponent from '../ui/CustomTabComponent';
import ProjectSubheader from '../project/projectSubheader';
import IntialReport from './intialReport';

const Reports = () => {
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Report', value: '' },
    { value: 'RSP', label: 'Sales & Purchase BI' },
    { value: 'RIB', label: 'Inventory BI' },
    { value: 'RPB', label: 'Production BI' },
    { value: 'RAS', label: 'Accounts BI' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  return (
    <div>
      <div>
        <ProjectSubheader navigation={'/home'} title="Reports & Intelligence" />
        <CustomTabComponent
          labels={buttonLabels}
          onClick={handleGroupButtonClick}
          activeTab={activeButton}
        />
      </div>
      <div style={{ padding: '20px' }}>
        {activeButton === '' ? <IntialReport /> : ''}
      </div>
    </div>
  );
};

export default Reports;
