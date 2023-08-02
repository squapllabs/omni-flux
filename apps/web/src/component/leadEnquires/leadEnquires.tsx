import React, { useState, useEffect } from 'react';
import Styles from '../../styles/leadTender.module.scss';
import SelectNew from '../ui/selectNew';
import ProductSale from './product/productSale';
import Tender from './tender/tender';

const leadEnquires = () => {
  const [selectedValue, setSelectedValue] = useState('PS');
  const leadType = [
    { value: 'PS', label: 'Product Sale' },
    { value: 'TD', label: 'Tender' },
  ];
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
  };
  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <span className={Styles.main_content}>Add -Lead/Enquiry</span>
            <span className={Styles.content}>Add your Lead & Enquiries</span>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.main_body}>
            <div style={{ width: '40%' }}>
              <SelectNew
                label="Parent Name"
                name="parent_master_data_id"
                defaultLabel="select the option"
                onChange={handleDropdownChange}
                value={selectedValue}
              >
                {leadType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </SelectNew>
            </div>
          </div>
          {selectedValue === 'PS' ? <ProductSale /> : <Tender />}
        </div>
      </div>
    </div>
  );
};

export default leadEnquires;
