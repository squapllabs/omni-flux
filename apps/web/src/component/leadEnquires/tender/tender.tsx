import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/leadTender.module.scss';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import DatePicker from '../../ui/CustomDatePicker';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';

const Tender = () => {
  const leadType = [
    { value: 'PS', label: 'Product Sale' },
    { value: 'TD', label: 'Tender' },
  ];
  const [selectedValue, setSelectedValue] = useState('');
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
  };
  return (
    <div>
      <div className={Styles.box}>
        <div className={Styles.fields_container}>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Input label="Lead #" />
            </div>
            <div className={Styles.fieldStyle}>
              <Input label="Tender Registration No." />
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Input label="Tender Identification #" />
            </div>
            <div className={Styles.fieldStyle}>
              <DatePicker label="Target Issued Date" />
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Input label="Tender Name" />
            </div>
            <div className={Styles.fieldStyle}>
              <DatePicker label="Target Due Date" />
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Select
                name="Tender"
                label="Tender Type"
                defaultLabel="select Client"
                value={selectedValue}
                onChange={handleDropdownChange}
              >
                {leadType?.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div className={Styles.fieldStyle}>
              <Input label="Estimate Value" />
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Select
                name="Tender"
                label="Industry/Sector"
                defaultLabel="select Client"
                value={selectedValue}
                onChange={handleDropdownChange}
              >
                {leadType?.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div className={Styles.fieldStyle}>
              {/* <Input label="Estimate Value" /> */}
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Select
                name="client"
                label="Client"
                defaultLabel="select Client"
                value={selectedValue}
                onChange={handleDropdownChange}
              >
                {leadType?.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div className={Styles.fieldStyle}>
              <Select
                name="Tender"
                label="Client Level"
                defaultLabel="select Client"
                value={selectedValue}
                onChange={handleDropdownChange}
              >
                {leadType?.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Input label="Client Contact Name" />
            </div>
            <div className={Styles.fieldStyle}>
              <Input label="Client Contact Email" />
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Input label="Client Contact Phone" />
            </div>
            <div className={Styles.fieldStyle}>
              {/* <Input label="Client Contact Email" /> */}
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}></div>
            <div className={Styles.button_container}>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                icon={<AddIcon />}
              >
                Add Tender
              </Button>
            </div>
          </div>
        </div>
      </div>
      <div className={Styles.box}></div>
    </div>
  );
};

export default Tender;
