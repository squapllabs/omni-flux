import React from 'react';
import Styles from '../../styles/expanses.module.scss';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import AddIcon from '../menu/icons/addIcon';

const ExpansesForm = () => {
  var tableInputwidth = '100px';
  return (
    <div>
      <div className={Styles.container}>
        <div>
          <div className={Styles.textContent}>
            <h3>Add Site Expanse</h3>
            <span className={Styles.content}>Add your site expanse.</span>
          </div>
          <div>
            <form>
              <div className={Styles.box}>
                <div className={Styles.fields_container}>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <Input label="EMP Name" />
                    </div>
                    <div className={Styles.fieldStyle}>
                      <Input label="EMP ID" />
                    </div>
                  </div>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <Input label="EMP Phone" />
                    </div>
                    <div className={Styles.fieldStyle}>
                      <Select
                        label="Purpose"
                        defaultLabel="select the option"
                      ></Select>
                    </div>
                  </div>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <Select
                        label="Department"
                        defaultLabel="select the option"
                      ></Select>
                    </div>
                    <div className={Styles.fieldStyle}>
                      <Select
                        label="Designation"
                        defaultLabel="select the option"
                      ></Select>
                    </div>
                  </div>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <DatePicker label="Start Date" />
                    </div>
                    <div className={Styles.fieldStyle}>
                      <DatePicker label="End Date" />
                    </div>
                  </div>
                </div>
              </div>
              <div className={Styles.box}>
                <div className={Styles.textContent}>
                  <h3>Expanse</h3>
                  <span className={Styles.content}>
                    List your site expanse.
                  </span>
                </div>
                <div className={Styles.table_container}>
                  <table className={Styles.scrollable_table}>
                    <thead>
                      <tr>
                        <th>S No</th>
                        <th>Description</th>
                        <th>Air/Transport</th>
                        <th>Fuel</th>
                        <th>Labour Advance</th>
                        <th>phone/Stationary</th>
                        <th>Food/snackes</th>
                        <th>Purchase Service</th>
                        <th>Others</th>
                        <th>Total</th>
                        <th>Action</th>
                      </tr>
                    </thead>
                    <tbody>
                      <tr>
                        <td></td>
                        <td>
                          {' '}
                          <Input width="100px" />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          {' '}
                          <Input width={tableInputwidth} />
                        </td>
                        <td>
                          <AddIcon />
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ExpansesForm;
