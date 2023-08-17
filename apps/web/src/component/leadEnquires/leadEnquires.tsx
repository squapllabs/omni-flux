import React, { useState, useEffect } from 'react';
import Styles from '../../styles/leadTender.module.scss';
import SelectNew from '../ui/selectNew';
import ProductSale from './product/productSale';
import Tender from './tender/tender';
import { useParams } from 'react-router-dom';
const leadEnquires = () => {
  const routeprops = useParams();
  const [selectedValue, setSelectedValue] = useState(
    routeprops.type != undefined ? routeprops.type : 'Product'
  );
  const leadType = [
    { value: 'Product', label: 'Product Sale' },
    { value: 'Tender', label: 'Tender' },
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
            <span className={Styles.main_content}>Add - Lead/Enquiry</span>
            <span className={Styles.content}>Add your Lead & Enquiries</span>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.main_body}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <SelectNew
                  label="Lead Type"
                  name="parent_master_data_id"
                  defaultLabel="select the option"
                  onChange={handleDropdownChange}
                  value={selectedValue}
                  helperText="Depending on selected Lead Type below form will change"
                  disabled={routeprops.type != undefined ? true : false}
                >
                  {leadType.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </SelectNew>
              </div>
            </div>
          </div>
          <div className={Styles.itemAdd}>
            {selectedValue === 'Product' ? (
              <ProductSale
                leadType={selectedValue}
                leadEnquireId={routeprops?.id}
              />
            ) : (
              <Tender leadType={selectedValue} leadEnquireId={routeprops?.id} />
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default leadEnquires;
