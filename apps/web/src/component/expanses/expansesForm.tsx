import React, { useEffect, useState } from 'react';
import Styles from '../../styles/expanses.module.scss';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useFormik } from 'formik';
import { getBymasertDataType } from '../../hooks/masertData-hook';
import PopupExpense from './popupExpanse';
import CustomDelete from '../ui/customDeleteDialogBox';
import {
  createsiteExpense,
  updatesiteExpense,
} from '../../hooks/siteExpanse-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import siteExpenseService from '../../service/siteExpanse-service';
import { format } from 'date-fns';
import * as Yup from 'yup';
import { getCreateValidateyup } from '../../helper/constants/siteExpanse-constants';
import CustomDialogBox from '../ui/CustomDialog';
import CustomSnackBar from '../ui/customSnackBar';
import { useParams, useNavigate } from 'react-router-dom';

const ExpansesForm = () => {
  const params = useParams();
  const navigate = useNavigate();
  let projectId = Number(params?.projectId);
  let siteId = Number(params?.siteId);
  const validationSchema = getCreateValidateyup(Yup);
  const state: RootState = store.getState();
  let encryptedData = getToken(state, 'Data');
  var tableInputwidth = '100px';
  let rowIndex = 0;
  let espanseObject: any = {
    site_expense_details_id: '',
    description: '',
    is_delete: 'N',
    air_transport: 0,
    fuel: 0,
    labour_advance: 0,
    phone_stationary: 0,
    food_snacks: 0,
    purchase_service: 0,
    others: 0,
    total: 0,
  };
  const [expenseList, setExpenseList] = useState<any>([]);
  const [expense, setExpense] = useState(espanseObject);
  const [ExpenseValue, setExpenseValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
  const [reload, setReload] = useState(false);
  const [errors, setErrors] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [initialValues, setInitialValues] = useState({
    employee_name: '',
    employee_id: '',
    employee_phone: '',
    end_date: '',
    start_date: '',
    purpose: '',
    department: '',
    designation: '',
    site_expense_id: '',
  });
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const handleCloseDialog = () => {
    setOpenDialog(false);
  };
  /* Function for closing the snackbar */
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const { data: getAllDiscription } = getBymasertDataType('SEDT');
  const { data: getAllSiteDepartment } = getBymasertDataType('SITD');
  const { data: getAllpurpose } = getBymasertDataType('SITP');
  const { data: getAlldesignation } = getBymasertDataType('SITDG');
  const { mutate: postSiteExpenseData, isLoading: postLoader } =
    createsiteExpense();
  const { mutate: updateSiteExpenseData, isLoading: updateLoader } =
    updatesiteExpense();

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    let formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  useEffect(() => {
    const fetchData = async () => {
      const postIds = {
        projectId: projectId,
        siteId: siteId,
      };
      const datas = await siteExpenseService.getSiteExpenseByProjectandSiteID(
        postIds
      );

      setInitialValues({
        employee_name: datas?.data?.employee_name,
        employee_id: datas?.data?.employee_id,
        employee_phone: datas?.data?.employee_phone,
        end_date: dateFormat(datas?.data?.end_date),
        start_date: dateFormat(datas?.data?.start_date),
        purpose: datas?.data?.purpose,
        department: datas?.data?.department,
        designation: datas?.data?.designation,
        site_expense_id: datas?.data?.site_expense_id,
      });
      let arry: any = [];
      datas?.data?.site_expense_details.map((items: any) => {
        items.is_delete = 'N';
        let demo = {
          site_expense_details_id: items.site_expense_details_id,
          description: items.description,
          is_delete: 'N',
          air_transport: items.air_transport,
          fuel: items.fuel,
          labour_advance: items.labour_advance,
          phone_stationary: items.phone_stationary,
          food_snacks: items.food_snacks,
          purchase_service: items.purchase_service,
          others: items.others,
          total: items.total,
        };

        arry.push(demo);
      });
      setExpenseList(arry);
    };
    fetchData();
  }, [reload]);
  const handleExpenseChange = async (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    let tempObj = {};
    if (event.target.name === 'description') {
      tempObj = {
        ...expense,
        [event.target.name]: event.target.value,
      };
    } else {
      tempObj = {
        ...expense,
        [event.target.name]: Number(event.target.value),
      };
      tempObj = {
        ...tempObj,
        total:
          tempObj?.air_transport +
          tempObj?.fuel +
          tempObj?.labour_advance +
          tempObj?.phone_stationary +
          tempObj?.food_snacks +
          tempObj?.purchase_service +
          tempObj?.others,
      };
    }
    setExpense(tempObj);
  };
  const handleExistExpenseChange = async (
    event: React.ChangeEvent<HTMLInputElement>,
    index: number
  ) => {
    let tempObj = {};
    if (event.target.name === 'description') {
      tempObj = {
        ...expenseList[index],
        [event.target.name]: event.target.value,
      };
    } else {
      tempObj = {
        ...expenseList[index],
        [event.target.name]: Number(event.target.value),
      };
      tempObj = {
        ...tempObj,
        total:
          tempObj?.air_transport +
          tempObj?.fuel +
          tempObj?.labour_advance +
          tempObj?.phone_stationary +
          tempObj?.food_snacks +
          tempObj?.purchase_service +
          tempObj?.others,
      };
    }
    let tempArry = [...expenseList];
    tempArry[index] = tempObj;
    setExpenseList(tempArry);
  };
  const handleExpenseAdd = async () => {
    setErrors({});
    const schema = Yup.object().shape({
      is_delete: Yup.string().required(),
      description: Yup.string()
        .typeError('Description is required')
        .required('Description is required')
        .test(
          'description-availability',
          'Description is already present',
          async function (value, { parent }: Yup.TestContext) {
            let isDelete = parent.is_delete;
            try {
              const isValuePresent = expenseList.some((obj) => {
                return obj.description === value && obj.is_delete === isDelete;
              });
              if (isValuePresent === true) {
                return false;
              } else {
                return true;
              }
            } catch {
              return true;
            }
          }
        ),
      air_transport: Yup.number(),
      fuel: Yup.number(),
      labour_advance: Yup.number(),
      phone_stationary: Yup.number(),
      food_snacks: Yup.number(),
      purchase_service: Yup.number(),
      others: Yup.number(),
      total: Yup.number(),
    });
    await schema
      .validate(expense, { abortEarly: false })
      .then(async () => {
        let tempArr = [];
        tempArr =
          expenseList !== undefined ? [...expenseList, expense] : [expense];
        setExpenseList(tempArr);
        setExpense({
          description: '',
          is_delete: 'N',
          air_transport: 0,
          fuel: 0,
          labour_advance: 0,
          phone_stationary: 0,
          food_snacks: 0,
          purchase_service: 0,
          others: 0,
          total: 0,
        });
      })
      .catch((e) => {
        let errorObj: any = {};
        e.inner.map((errors: any) => {
          return (errorObj[errors.path] = errors.message);
        });
        setErrors({
          ...errorObj,
        });
      });
  };

  const handleDeleteSiteExpense = (e: any, value: any) => {
    setExpenseValue(value);
    setOpenDelete(true);
  };
  const deleteSiteExpense = (e: any, values: any) => {
    const itemIndex = expenseList.findIndex(
      (item: any) =>
        item.description === ExpenseValue?.description &&
        item.is_delete === ExpenseValue?.is_delete
    );
    expenseList[itemIndex] = {
      ...expenseList[itemIndex],
      is_delete: 'Y',
    };
    setExpenseList([...expenseList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
    setMessage('Site expanse details has been deleted');
    setOpenSnack(true);
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      let count = 0;
      for (let i = 0; i < expenseList.length; i++) {
        if (expenseList[i].is_delete === 'N') {
          count++;
        }
      }
      if (count === 0) {
        setOpenDialog(true);
      } else {
        if (values.site_expense_id === '') {
          const object: any = {
            site_id: siteId,
            project_id: projectId,
            employee_name: values.employee_name,
            employee_id: values.employee_id,
            employee_phone: values.employee_phone,
            end_date: values.end_date,
            start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            site_expense_details: expenseList,
            created_by: encryptedData?.userId,
          };
          postSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              console.log('data', data);

              if (data?.status === true) {
                setMessage('Site Expense has been added successfully !');
                setOpenSnack(true);
                setInterval(() => {
                  navigate(`/project-edit/${projectId}`);
                }, 3000);
              }
            },
          });
        } else {
          const object: any = {
            site_id: siteId,
            project_id: projectId,
            employee_name: values.employee_name,
            employee_id: values.employee_id,
            employee_phone: values.employee_phone,
            end_date: values.end_date,
            start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            site_expense_details: expenseList,
            created_by: encryptedData?.userId,
            updated_by: encryptedData?.userId,
            site_expense_id: values.site_expense_id,
          };
          updateSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              if (data?.status === true) {
                setMessage('Site Expense has been updated successfully !');
                setOpenSnack(true);
                setInterval(() => {
                  navigate(`/project-edit/${projectId}`);
                }, 3000);
              }
            },
          });
        }
      }
    },
  });
  return (
    <div>
      <div className={Styles.container}>
        <div>
          <div className={Styles.textContent}>
            <h3>Add Site Expense</h3>
            <span className={Styles.content}>Add your site expense.</span>
          </div>
        </div>
      </div>
      <form>
        <div className={Styles.box}>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="EMP Name"
                  name="employee_name"
                  onChange={formik.handleChange}
                  value={formik.values.employee_name}
                  mandatory
                  error={
                    formik.touched.employee_name && formik.errors.employee_name
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Input
                  label="EMP ID"
                  name="employee_id"
                  onChange={formik.handleChange}
                  value={formik.values.employee_id}
                  mandatory
                  error={
                    formik.touched.employee_id && formik.errors.employee_id
                  }
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="EMP Phone"
                  name="employee_phone"
                  onChange={formik.handleChange}
                  value={formik.values.employee_phone}
                  error={
                    formik.touched.employee_phone &&
                    formik.errors.employee_phone
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  label="Purpose"
                  defaultLabel="select a Purpose"
                  name="purpose"
                  onChange={formik.handleChange}
                  value={formik.values.purpose}
                  mandatory
                  error={formik.touched.purpose && formik.errors.purpose}
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
                  value={formik.values.department}
                  mandatory
                  error={formik.touched.department && formik.errors.department}
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
                  value={formik.values.designation}
                  error={
                    formik.touched.designation && formik.errors.designation
                  }
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
                  value={formik.values.start_date}
                  mandatory
                  error={formik.touched.start_date && formik.errors.start_date}
                />
              </div>
              <div className={Styles.fieldStyle}>
                <DatePicker
                  label="End Date"
                  name="end_date"
                  onChange={formik.handleChange}
                  value={formik.values.end_date}
                  mandatory
                  error={formik.touched.end_date && formik.errors.end_date}
                />
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Expense</h3>
            <span className={Styles.content}>List your site expense.</span>
          </div>
          <div className={Styles.bulkComponent}>
            <PopupExpense
              setExpenseList={setExpenseList}
              projectId={projectId}
              siteId={siteId}
              userId={encryptedData?.userId}
              setReload={setReload}
              reload={reload}
              openSnack={openSnack}
              setOpenSnack={setOpenSnack}
              message={message}
              setMessage={setMessage}
            />
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
                {expenseList?.map((item: any, index: any) => {
                  if (item?.is_delete === 'N') {
                    rowIndex = rowIndex + 1;
                    return (
                      
                        <tr>
                          <td>{rowIndex}</td>
                          <td>
                            <div
                              style={{
                                paddingBottom: '20px',
                                fontSize: '15px',
                              }}
                            >
                              <span>{item?.description}</span>
                            </div>
                          </td>
                          <td>
                            <Input
                              width={tableInputwidth}
                              name="air_transport"
                              onChange={(e) =>
                                handleExistExpenseChange(e, index)
                              }
                              value={item?.air_transport}
                            />
                          </td>
                          <td>
                            <Input
                              width={tableInputwidth}
                              name="fuel"
                              onChange={(e) =>
                                handleExistExpenseChange(e, index)
                              }
                              value={item?.fuel}
                            />
                          </td>
                          <td>
                            <Input
                              width={tableInputwidth}
                              name="labour_advance"
                              onChange={(e) =>
                                handleExistExpenseChange(e, index)
                              }
                              value={item?.labour_advance}
                            />
                          </td>
                          <td>
                            <Input
                              width={tableInputwidth}
                              name="phone_stationary"
                              onChange={(e) =>
                                handleExistExpenseChange(e, index)
                              }
                              value={item?.phone_stationary}
                            />
                          </td>
                          <td>
                            <Input
                              width={tableInputwidth}
                              name="food_snacks"
                              onChange={(e) =>
                                handleExistExpenseChange(e, index)
                              }
                              value={item?.food_snacks}
                            />
                          </td>
                          <td>
                            <Input
                              width={tableInputwidth}
                              name="purchase_service"
                              onChange={(e) =>
                                handleExistExpenseChange(e, index)
                              }
                              value={item?.purchase_service}
                            />
                          </td>
                          <td>
                            <Input
                              width={tableInputwidth}
                              onChange={(e) =>
                                handleExistExpenseChange(e, index)
                              }
                              name="others"
                              value={item?.others}
                            />
                          </td>
                          <td>
                            <div
                              style={{
                                paddingBottom: '20px',
                                fontSize: '15px',
                                fontWeight: 'bold',
                              }}
                            >
                              <span>{expenseList[index].total}</span>
                            </div>
                          </td>
                          <td>
                            <div
                              style={{
                                cursor: 'pointer',
                                paddingBottom: '20px',
                              }}
                            >
                              <DeleteIcon
                                onClick={(e: any) =>
                                  handleDeleteSiteExpense(e, item)
                                }
                              />
                            </div>
                          </td>
                        </tr>
                    
                    );
                  }
                })}
                <tr>
                  <td>{rowIndex + 1}</td>
                  <td>
                    <Select
                      width="200px"
                      onChange={(e) => handleExpenseChange(e)}
                      name="description"
                      defaultLabel="select a Description"
                      mandatory
                      value={expense.description || ''}
                      error={errors.description}
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
                      onChange={(e) => handleExpenseChange(e)}
                      value={expense.air_transport || ''}
                    />
                  </td>
                  <td>
                    <Input
                      width={tableInputwidth}
                      name="fuel"
                      onChange={(e) => handleExpenseChange(e)}
                      value={expense.fuel || ''}
                    />
                  </td>
                  <td>
                    <Input
                      width={tableInputwidth}
                      name="labour_advance"
                      onChange={(e) => handleExpenseChange(e)}
                      value={expense.labour_advance || ''}
                    />
                  </td>
                  <td>
                    <Input
                      width={tableInputwidth}
                      name="phone_stationary"
                      onChange={(e) => handleExpenseChange(e)}
                      value={expense.phone_stationary || ''}
                    />
                  </td>
                  <td>
                    <Input
                      width={tableInputwidth}
                      name="food_snacks"
                      onChange={(e) => handleExpenseChange(e)}
                      value={expense.food_snacks || ''}
                    />
                  </td>
                  <td>
                    <Input
                      width={tableInputwidth}
                      name="purchase_service"
                      onChange={(e) => handleExpenseChange(e)}
                      value={expense.purchase_service || ''}
                    />
                  </td>
                  <td>
                    <Input
                      width={tableInputwidth}
                      onChange={(e) => handleExpenseChange(e)}
                      name="others"
                      value={expense.others || ''}
                    />
                  </td>
                  <td style={{ textAlign: 'center' }}>
                    <span className={Styles.totalExpense}>{expense.total}</span>
                  </td>
                  <td style={{ textAlign: 'center' }}>
                    <div
                      style={{
                        cursor: 'pointer',
                        width: tableInputwidth,
                        display: 'none',
                      }}
                    >
                      <DeleteIcon
                        onClick={(e: any) => handleDeleteSiteExpense(e)}
                      />
                    </div>
                  </td>
                </tr>
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
              onClick={(e) => handleExpenseAdd()}
            >
              ADD
            </Button>
          </div>
          <div className={Styles.submitButton}>
            <Button
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
              type="button"
              onClick={formik.handleSubmit}
            >
              Submit
            </Button>
          </div>
        </div>
      </form>
      <CustomDelete
        open={openDelete}
        title="Delete Site Expense"
        contentLine1="Are you sure you want to delete this Expense ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteSiteExpense}
      />

      <CustomDialogBox
        open={openDialog}
        title="Warning"
        contentLine1="Please add site expanse details"
        contentLine2=""
        handleClose={handleCloseDialog}
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default ExpansesForm;
