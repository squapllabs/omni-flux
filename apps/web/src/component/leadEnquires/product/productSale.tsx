import React, { useState, useEffect } from 'react';
import Styles from '../../../styles/leadTender.module.scss';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import TextArea from '../../ui/CustomTextArea';

const ProductSale = () => {
  const [selectedValue, setSelectedValue] = useState('');
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
      <div className={Styles.box}>
        <div className={Styles.fields_container}>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Input label="Lead #" />
            </div>
            <div className={Styles.fieldStyle}>
              <Select
                name="client"
                label="Client"
                defaultLabel="select Client"
                onChange={handleDropdownChange}
                value={selectedValue}
              >
                {leadType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Select
                name="client"
                label="Lead Source"
                defaultLabel="select a Lead Source"
                onChange={handleDropdownChange}
                value={selectedValue}
              >
                {leadType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div className={Styles.fieldStyle}>
              <Select
                name="client"
                label="Client Level"
                defaultLabel="select a Client Level"
                onChange={handleDropdownChange}
                value={selectedValue}
              >
                {leadType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              {' '}
              <Select
                name="client"
                label="Lead Probability"
                defaultLabel="select a Lead Probability"
                onChange={handleDropdownChange}
                value={selectedValue}
              >
                {leadType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div className={Styles.fieldStyle}>
              <Input label="Client Contact Name" />
            </div>
          </div>
        </div>
      </div>
      <div className={Styles.box}>
        <div className={Styles.container_2}>
          <table>
            <thead>
              <tr>
                <th>S No</th>
                <th>Name</th>
                <th>Description</th>
                <th>Code</th>
                <th>Parent Name</th>
                <th>option</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
      <div className={Styles.box}>
        <div className={Styles.fields_container}>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <Input label="Approx value" />
            </div>
            <div className={Styles.fieldStyle}>
              <Select
                name="client"
                label="Sales person Name"
                defaultLabel="select Client"
                onChange={handleDropdownChange}
                value={selectedValue}
              >
                {leadType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.fieldStyle}>
              <TextArea label="Client Remarks" />
            </div>
            <div className={Styles.fieldStyle}>
              <TextArea label="Our Remarks" />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProductSale;
