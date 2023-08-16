import React, { useState } from 'react';
import Styles from '../../styles/expanses.module.scss';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import AddIcon from '../menu/icons/addIcon';
import Button from '../ui/Button';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useFormik } from 'formik';
import { getBymasertDataType } from 'apps/web/src/hooks/masertData-hook';

interface Expanse {
  description: String;
  air_transport: number;
  fuel: number;
  labour_advance: number;
  phone_stationary: number;
  food_snacks: number;
  purchase_service: number;
  others: number;
}
const ExpansesForm = () => {
  var tableInputwidth = '100px';
  let rowIndex = 0;
  const espanseObject: any = {
    description: '',
    air_transport: '',
    fuel: '',
    labour_advance: '',
    phone_stationary: '',
    food_snacks: '',
    purchase_service: '',
    others: '',
    total: 0,
  };
  const [expanseList, setExpanseList] = useState<Expanse[]>([espanseObject]);
  const [expanse, setExpanse] = useState(espanseObject);
  const [initialValues, setInitialValues] = useState({
    employee_name: '',
    employee_id: '',
    employee_phone: '',
    end_date: '',
    start_date: '',
    purpose: '',
    department: '',
    designation: '',
  });

  const { data: getAllDiscription } = getBymasertDataType('SEDT');
  const { data: getAllSiteDepartment } = getBymasertDataType('SITD');
  const { data: getAllpurpose } = getBymasertDataType('SITP');
  const { data: getAlldesignation } = getBymasertDataType('SITDG');

  const handleExpanseChange = (event: any, index: number) => {
    let tempObj = { ...expanse, [event.target.name]: event.target.value };
    if (event.target.name != 'description') {
      let data = Number(event.target.value) + Number(expanse.total);
      console.log('data check===>', data);
      tempObj['total'] = data;
    }
    let tempArr = [...expanseList];
    tempArr[index] = tempObj;
    setExpanse(tempObj);
    setExpanseList([...tempArr]);
    formik.setFieldValue('site_expense_details', expanseList);
  };
  console.log('expanseList', expanseList);
  const handleExpanseAdd = () => {
    setExpanseList([...expanseList, expanse]);
  };

  const formik = useFormik({
    initialValues,
    enableReinitialize: true,
    onSubmit: (values) => {
      console.log(values);
    },
  });
  return (
    <div>
      <div className={Styles.container}>
        <div>
          <div className={Styles.textContent}>
            <h3>Add Site Expanse</h3>
            <span className={Styles.content}>Add your site expanse.</span>
          </div>
          <div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.box}>
                <div className={Styles.fields_container}>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <Input
                        label="EMP Name"
                        name="employee_name"
                        onChange={formik.handleChange}
                      />
                    </div>
                    <div className={Styles.fieldStyle}>
                      <Input
                        label="EMP ID"
                        name="employee_id"
                        onChange={formik.handleChange}
                      />
                    </div>
                  </div>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <Input
                        label="EMP Phone"
                        name="employee_phone"
                        onChange={formik.handleChange}
                      />
                    </div>
                    <div className={Styles.fieldStyle}>
                      <Select
                        label="Purpose"
                        defaultLabel="select a Purpose"
                        name="purpose"
                        onChange={formik.handleChange}
                      >
                        {getAllpurpose?.map((option: any) => (
                          <option
                            key={option.master_data_name}
                            value={option.master_data_name}
                          >
                            {option.master_data_name}
                          </option>
                        ))}
                      </Select>
                    </div>
                  </div>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <Select
                        label="Department"
                        defaultLabel="select a Department"
                        name="department"
                        onChange={formik.handleChange}
                      >
                        {getAllSiteDepartment?.map((option: any) => (
                          <option
                            key={option.master_data_name}
                            value={option.master_data_name}
                          >
                            {option.master_data_name}
                          </option>
                        ))}
                      </Select>
                    </div>
                    <div className={Styles.fieldStyle}>
                      <Select
                        label="Designation"
                        name="designation"
                        defaultLabel="select a Designation"
                        onChange={formik.handleChange}
                      >
                        {getAlldesignation?.map((option: any) => (
                          <option
                            key={option.master_data_name}
                            value={option.master_data_name}
                          >
                            {option.master_data_name}
                          </option>
                        ))}
                      </Select>
                    </div>
                  </div>
                  <div className={Styles.fields_container_1}>
                    <div className={Styles.fieldStyle}>
                      <DatePicker
                        label="Start Date"
                        name="start_date"
                        onChange={formik.handleChange}
                      />
                    </div>
                    <div className={Styles.fieldStyle}>
                      <DatePicker
                        label="End Date"
                        name="end_date"
                        onChange={formik.handleChange}
                      />
                    </div>
                  </div>
                </div>
              </div>
              <div className={Styles.box}>
                <from>
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
                        {expanseList?.map((item: any, index: any) => {
                          console.log('item', item?.description);
                          rowIndex = rowIndex + 1;
                          return (
                            <>
                              <tr>
                                <td>{rowIndex}</td>
                                <td style={{ width: '50%' }}>
                                  <Select
                                    width="100%"
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                    name="description"
                                    defaultLabel="select a Description"
                                  >
                                    {getAllDiscription?.map((option: any) => (
                                      <option
                                        key={option.master_data_name}
                                        value={option.master_data_name}
                                      >
                                        {option.master_data_name}
                                      </option>
                                    ))}
                                  </Select>
                                </td>
                                <td>
                                  <Input
                                    width={tableInputwidth}
                                    name="air_transport"
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                  />
                                </td>
                                <td>
                                  {' '}
                                  <Input
                                    width={tableInputwidth}
                                    name="fuel"
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                  />
                                </td>
                                <td>
                                  <Input
                                    width={tableInputwidth}
                                    name="labour_advance"
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                  />
                                </td>
                                <td>
                                  <Input
                                    width={tableInputwidth}
                                    name="phone_stationary"
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                  />
                                </td>
                                <td>
                                  <Input
                                    width={tableInputwidth}
                                    name="food_snacks"
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                  />
                                </td>
                                <td>
                                  <Input
                                    width={tableInputwidth}
                                    name="purchase_service"
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                  />
                                </td>
                                <td>
                                  <Input
                                    width={tableInputwidth}
                                    onChange={(e) =>
                                      handleExpanseChange(e, index)
                                    }
                                    name="others"
                                  />
                                </td>
                                <td>
                                  <Input
                                    width={tableInputwidth}
                                    name="total"
                                    // onChange={handleExpanseChange}
                                    value={expanseList[index].total}
                                  />
                                </td>
                                <td>
                                  <DeleteIcon />
                                </td>
                              </tr>
                            </>
                          );
                        })}
                      </tbody>
                    </table>
                  </div>
                  <div className={Styles.expanseAddButton}>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      type="button"
                      onClick={handleExpanseAdd}
                    >
                      ADD
                    </Button>
                  </div>
                </from>
                <div className={Styles.submitButton}>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    // type="button"
                  >
                    Submit
                  </Button>
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
