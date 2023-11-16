import React, { useRef, useState, useEffect } from 'react';
import Styles from '../../../../styles/newStyles/projectSiteExpense.module.scss';
import Input from '../../../ui/Input';
import { useGetBymasertDataTypeDrop } from '../../../../hooks/masertData-hook';
import * as Yup from 'yup';
import { useFormik } from 'formik';
import DeleteIcon from '../../../menu/icons/newDeleteIcon';
import CustomDelete from '../../../ui/customDeleteDialogBox';
import NewAddCircleIcon from '../../../menu/icons/newAddCircleIcon';
import FileUploadIcon from '../../../menu/icons/fileUploadIcon';
import userService from '../../../../service/user-service';
import Popup from '../../../ui/CustomPdfPopup';
import SiteExpensesView from './siteExpensesView';
import CloseIcon from '../../../menu/icons/closeIcon';
import Select from '../../../ui/selectNew';

const SiteExpensesDetails: React.FC = (props: any) => {
  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    expense_data_id: '',
    site_expense_name: '',
    bill_number: '',
    total: '',
    is_delete: false,
    description: '',
    bill_details: [],
    bill_type: '',
    status: '',
  });
  const options = [
    { value: 'CASH', label: 'Cash' },
    { value: 'BILL', label: 'Bill' },
    { value: 'VOUCHER', label: 'Voucher' },
  ];
  const [openPdfpopup, setOpenPdfpopup] = useState(false);
  const [viewDocs, setViewDocs] = useState();
  const [ExpenseValue, setExpenseValue] = useState<any>({});
  const [openDelete, setOpenDelete] = useState(false);
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const fileInputRef_2 = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [display, setDisplay] = useState(props.mode === 'Add' ? true : false);
  const [fileMandatoryError, setFileMandatoryError] = useState('');
  const { data: getSiteExpense } = useGetBymasertDataTypeDrop('SIEP');
  const [expenseIndex, setExpenseIndex] = useState<any>();
  const [screenSize, setScreenSize] = useState(getCurrentDimension());
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  function getCurrentDimension() {
    return {
      width: window.innerWidth,
      height: window.innerHeight,
    };
  }

  useEffect(() => {
    const updateDimension = () => {
      setScreenSize(getCurrentDimension());
    };
    window.addEventListener("resize", updateDimension);

    return () => {
      window.removeEventListener("resize", updateDimension);
    };
  }, [screenSize]);


  useEffect(() => {
    if (props?.mode != 'Edit' && props.expenseList.length === 0) {
      props.setExpenseList([...props.expenseList, initialValues]);
    }
  }, [props?.mode]);

  const deleteSiteExpense = (e: any, values: any) => {
    if (props.expenseList[expenseIndex].expense_details_id != null) {
      if (props.expenseList[expenseIndex].bill_details != '') {
        props.expenseList[expenseIndex] = {
          ...props.expenseList[expenseIndex],
          is_delete: true,
          bill_details: props.expenseList[expenseIndex].bill_details.map(
            (billDetail: any, index: number) => {
              if (index === 0) {
                return { ...billDetail, is_delete: 'Y' };
              }
              return billDetail;
            }
          ),
        };
      } else {
        props.expenseList[expenseIndex] = {
          ...props.expenseList[expenseIndex],
          is_delete: true,
        };
      }
    } else {
      props.expenseList.splice(expenseIndex, 1);
    }

    props.setExpenseList([...props.expenseList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
    props.setMessage('Site expanse details has been deleted');
    props.setOpenSnack(true);
  };

  const validationSchema = Yup.array().of(
    Yup.object().shape({
      is_delete: Yup.string().required(),
      expense_data_id: Yup.string()
        .typeError('Site Expense is required')
        .required('Site Expense is required'),
      total: Yup.number()
        .min(1, 'Amount must be more then 0')
        .max(100000, 'Amount must be less then 100000')
        .typeError('Only Numbers are allowed')
        .required('Amount is required'),
      description: Yup.string().required('Description is required'),
      bill_number: Yup.string(),
    })
  );

  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    let tempObj = {};

    if (event.target.name === 'total') {
      tempObj = {
        ...props.expenseList[index],
        [event.target.name]: Number(event.target.value),
      };
    } else if (event.target.name === 'expense_data_id') {
      const matchingObjects = getSiteExpense.filter(
        (obj: any) => Number(obj.value) === Number(event.target.value)
      );
      tempObj = {
        ...props.expenseList[index],
        expense_data_id: matchingObjects[0].value,
        site_expense_name: matchingObjects[0].label,
      };

      let tempArry = [...props.expenseList];
      tempArry[index] = tempObj;
      props.setExpenseList(tempArry);
    } else {
      tempObj = {
        ...props.expenseList[index],
        [event.target.name]: event.target.value,
      };
    }
    let tempArry = [...props.expenseList];
    tempArry[index] = tempObj;
    props.setExpenseList(tempArry);
  };
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      if (values.bill_number != '' && selectedFiles.length === 0) {
        props.setMessage('Bill number is given but bill is not attached');
        props.setOpenSnack(true);
      } else {
        const code = 'SITEEXPENSE' + props.siteId;
        const s3UploadUrl: any = await handleDocuments(
          selectedFiles,
          code.toUpperCase()
        );
        formik.setFieldValue('bill_details', s3UploadUrl);
        values['total'] = Number(values.total);
        values['bill_details'] = s3UploadUrl;
        values['status'] = 'Pending';
        props.setExpenseList([...props.expenseList, values]);
        setSelectedFileName([]);
        resetForm();
        setSelectedFiles([]);
      }
    },
  });
  const generateCustomQuotationName = (data: any) => {
    if (data) {
      const vendorName =
        data?.site_expense_name === undefined
          ? data?.expense_master_data?.master_data_name
          : data?.site_expense_name || '';
      const year = new Date().getFullYear();
      const customBillName = `ALM-${vendorName.substring(0, 5)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };
  const onButtonClickRow = () => {
    if (fileInputRef_2.current) {
      fileInputRef_2.current.click();
    }
  };

  const viewDocumnet = (value: any) => {
    setOpenPdfpopup(true);
    setViewDocs(value);
  };

  const handleFileSelect = async (e: any) => {
    const files = e.target.files;
    props.setLoader(!props.loader);
    if (files.length > 0) {
      const fileList: File[] = Array.from(files);
      const oversizedFiles = fileList.filter(
        (file) => file.size > 10 * 1024 * 1024
      );
      if (oversizedFiles.length > 0) {
        const oversizedFileNames = oversizedFiles
          .map((file) => file.name)
          .join(', ');
        props.setLoader(!props.loader);
        const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
        setFileSizeError(errorMessage);
        props.setMessage(errorMessage);
        props.setOpenSnack(true);
      } else {
        const selectedFilesArray: File[] = [];
        const selectedFileNamesArray: string[] = [];
        fileList.forEach((file) => {
          selectedFilesArray.push(file);
          selectedFileNamesArray.push(file.name);
        });
        setSelectedFiles(selectedFilesArray);
        setSelectedFileName(selectedFileNamesArray);
        setFileSizeError('');
        props.setLoader(false);
        props.setMessage('Document uploaded');
        props.setOpenSnack(true);
      }
    }
  };
  const handleFileSelectRow = async (e: any, index: any) => {
    const files = e.target.files;
    props.setLoader(!props.loader);
    if (files.length > 0) {
      const fileList: File[] = Array.from(files);
      const oversizedFiles = fileList.filter(
        (file) => file.size > 10 * 1024 * 1024
      );
      if (oversizedFiles.length > 0) {
        const oversizedFileNames = oversizedFiles
          .map((file) => file.name)
          .join(', ');
        props.setLoader(!props.loader);
        const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
        setFileSizeError(errorMessage);
        props.setMessage(errorMessage);
        props.setOpenSnack(true);
      } else {
        const selectedFilesArray: File[] = [];
        const selectedFileNamesArray: string[] = [];
        let arr: any = [];
        fileList.forEach(async (file) => {
          const code = 'SITEEXPENSE' + props.siteId;
          const response = await userService.documentUpload(file, code);

          let obj = {
            ...response?.data[0],
            is_delete: 'N',
          };
          arr.push(obj);
          selectedFilesArray.push(file);
          selectedFileNamesArray.push(file.name);
        });
        let tempObj = {};
        props.expenseList[expenseIndex].bill_details = arr;
        tempObj = {
          ...props.expenseList[expenseIndex],
        };
        let tempArry = [...props.expenseList];
        tempArry[expenseIndex] = tempObj;
        props.setExpenseList(tempArry);
        setSelectedFiles(selectedFilesArray);
        setSelectedFileName(selectedFileNamesArray);
        setFileSizeError('');
        props.setLoader(false);
        props.setMessage('Document uploaded');
        props.setOpenSnack(true);
      }
    }
  };
  const handleDocuments = async (files: File[], code: string) => {
    try {
      const uploadPromises = files.map(async (file) => {
        const response = await userService.documentUpload(file, code);
        return response.data;
      });
      const uploadResponses = await Promise.all(uploadPromises);
      const modifiedArray = uploadResponses.flatMap((response) => response);
      const modifiedArrayWithDeleteFlag = modifiedArray.map((obj) => ({
        ...obj,
        is_delete: 'N',
      }));
      return modifiedArrayWithDeleteFlag;
    } catch (error) {
      console.log('Error in occur project document upload:', error);
    }
  };
  const deleteFileinList = (index: any) => {
    let tempObj = {};
    props.expenseList[index].bill_details[0].is_delete = 'Y';
    props.expenseList[index].bill_number = '';
    tempObj = {
      ...props.expenseList[index],
    };
    let tempArry = [...props.expenseList];
    tempArry[index] = tempObj;
    props.setExpenseList(tempArry);
  };

  const handleAddObject = async () => {
    const schema = Yup.array().of(
      Yup.object().shape({
        is_delete: Yup.string().required(),
        expense_data_id: Yup.string()
          .typeError('Site Expense is required')
          .required('Site Expense is required'),
        total: Yup.number()
          .min(1, 'Amount must be more then 0')
          .typeError('Only Numbers are allowed')
          .required('Amount is required')
          .test(
            'description-availability',
            '',
            async function (value, { parent }: Yup.TestContext) {
              let bill_type = parent.bill_type;
              if (bill_type === 'VOUCHER' && value > 5000) {
                props.setMessage(
                  'In bill type voucher amount should not be more then 50000'
                );
                props.setOpenSnack(true);
                return false;
              } else {
                return true;
              }
            }
          ),
        description: Yup.string().required('Description is required'),
        bill_number: Yup.string(),
        bill_type: Yup.string().required('Bill type is required'),
        bill_details: Yup.array()
          .required()
          .test(
            'description-availability',
            'Site Expense is already present',
            async function (value, { parent }: Yup.TestContext) {
              let bill_details = parent.bill_details;
              if (
                bill_details?.length < 0 &&
                bill_details[0]?.is_delete === 'Y'
              ) {
                return true;
              } else if (
                bill_details?.length > 0 &&
                bill_details[0]?.is_delete === 'N'
              ) {
                return true;
              } else {
                props.setMessage('Bill is Missing');
                props.setOpenSnack(true);
                return false;
              }
            }
          ),
      })
    );
    await schema
      .validate(props.expenseList, { abortEarly: false })
      .then(async () => {
        props.setErrors({});
        props.setExpenseList([...props.expenseList, initialValues]);
      })
      .catch((e: any) => {
        let errorObj = {};
        e.inner?.map((error: any) => {
          return (errorObj[error.path] = error.message);
        });
        props.setErrors({
          ...errorObj,
        });
      });
  };

  return (
    <div>
      <form>
        <div className={Styles.fields_container}></div>
      </form>
      {screenSize.width > 750 && (
        <div>
          <div className={Styles.table_container}>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th>#</th>
                  <th>Description</th>
                  <th>Site Expense</th>
                  <th>Bill Type</th>
                  <th>Bill/Voucher No</th>
                  <th>Amount</th>
                  <th>Document</th>
                  <th>Action</th>
                </tr>
              </thead>
              <tbody>
                {props.expenseList?.map((item: any, index: any) => {
                  if (item.is_delete === false) {
                    rowIndex = rowIndex + 1;
                    const customQuotationName = generateCustomQuotationName(item);
                    return (
                      <tr>
                        <td>{rowIndex}</td>
                        <td>
                          <Input
                            name="description"
                            onChange={(e) => handleListChange(e, index)}
                            value={item.description}
                            width="300px"
                            error={
                              props.errors?.[`[${index}].description`]
                                ? true
                                : props.errors?.[`[${index}].bill_details`]
                                  ? true
                                  : false
                            }
                          />
                        </td>
                        <td>
                          <Select
                            width="180px"
                            name="expense_data_id"
                            mandatory={true}
                            defaultLabel="Select the option"
                            placeholder="Select the option"
                            value={item.expense_data_id}
                            onChange={(e) => handleListChange(e, index)}
                            error={
                              props.errors?.[`[${index}].expense_data_id`]
                                ? true
                                : props.errors?.[`[${index}].bill_details`]
                                  ? true
                                  : false
                            }
                          >
                            {getSiteExpense?.map((item: any, index: any) => {
                              return (
                                <option value={item.value}>{item.label}</option>
                              );
                            })}
                          </Select>
                        </td>
                        <td>
                          <Select
                            name="bill_type"
                            value={item.bill_type}
                            width="180px"
                            onChange={(e) => handleListChange(e, index)}
                            defaultLabel="Select the option"
                            placeholder="Select the option"
                            error={
                              props.errors?.[`[${index}].bill_type`]
                                ? true
                                : props.errors?.[`[${index}].bill_details`]
                                  ? true
                                  : false
                            }
                          >
                            {options?.map((item, index) => {
                              return (
                                <option value={item.value} >{item.label}</option>
                              );
                            })}
                          </Select>
                        </td>
                        <td>
                          <Input
                            name="bill_number"
                            value={item.bill_number}
                            onChange={(e) => handleListChange(e, index)}
                            width="160px"
                            error={
                              props.errors?.[`[${index}].bill_details`]
                                ? true
                                : false
                            }
                          />
                        </td>
                        <td style={{ overflow: 'hidden' }}>
                          <Input
                            name="total"
                            value={item.total}
                            onChange={(e) => handleListChange(e, index)}
                            width="120px"
                            error={
                              props.errors?.[`[${index}].total`]
                                ? true
                                : props.errors?.[`[${index}].bill_details`]
                                  ? true
                                  : false
                            }
                          />
                        </td>
                        <td>
                          {item.bill_details?.length > 0 &&
                            item.bill_details[0].is_delete === 'N' ? (
                            item.bill_details.map(
                              (document: any, billIndex: number) => {
                                if (document.is_delete === 'N')
                                  return (
                                    <div
                                      key={document.code}
                                      style={{
                                        width: '150px',
                                        cursor: 'pointer',
                                        fontWeight: 'bolder',
                                        color: 'blue',
                                        display: 'flex',
                                      }}
                                    >
                                      <div
                                        onClick={() => {
                                          viewDocumnet(item);
                                        }}
                                      >
                                        {customQuotationName}
                                      </div>
                                      <CloseIcon
                                        width={5}
                                        height={10}
                                        onClick={() => deleteFileinList(index)}
                                      />
                                    </div>
                                  );
                              }
                            )
                          ) : (
                            <div>
                              <div title="Attach document">
                                <input
                                  ref={fileInputRef_2}
                                  id="upload-photo"
                                  name="upload_photo"
                                  type="file"
                                  style={{ display: 'none' }}
                                  onChange={(e) => handleFileSelectRow(e, index)}
                                  error={
                                    formik.touched.bill_number &&
                                    formik.errors.bill_number
                                  }
                                />
                                <div
                                  style={{
                                    cursor: 'pointer',
                                    paddingBottom: '5px',
                                  }}
                                  onClick={() => {
                                    setExpenseIndex(index);
                                    onButtonClickRow();
                                  }}
                                >
                                  <FileUploadIcon color="#7f56d9" />
                                </div>
                                {fileMandatoryError && (
                                  <div className={Styles.documentErr}>
                                    {fileMandatoryError}
                                  </div>
                                )}
                              </div>
                            </div>
                          )}
                        </td>
                        <td>
                          <div className={Styles.buttons}>
                            <div
                              style={{ cursor: 'pointer' }}
                              onClick={() => {
                                setExpenseValue(item);
                                setOpenDelete(true);
                                setExpenseIndex(index);
                              }}
                            >
                              <DeleteIcon />
                            </div>
                          </div>
                        </td>
                      </tr>
                    );
                  }
                })}
              </tbody>
            </table>
          </div>
          <div className={Styles.addDataIcon}>
            <div
              onClick={() => {
                handleAddObject();
              }}
              className={Styles.iconContent}
            >
              <NewAddCircleIcon />
              <span>
                {props.expenseList.length > 0 ? 'Add More Claim' : 'Add Claim'}
              </span>
            </div>
          </div>
        </div>
      )}
      {screenSize.width <= 750 && (
        <div className={Styles.formContainerForMobile}>
          <div className={Styles.card}>
            <div className={Styles.cardDiv}>
              <table className={Styles.scrollable_table}>
                <tbody>
                  {props.expenseList?.map((item: any, index: any) => {
                    if (item.is_delete === false) {
                      rowIndex = rowIndex + 1;
                      const customQuotationName = generateCustomQuotationName(item);
                      return (
                        <tr style={{ display: 'flex', flexDirection: 'column' }}>
                          <div className={Styles.tableBody}>
                            <div>
                              <Input
                                name="description"
                                onChange={(e) => handleListChange(e, index)}
                                value={item.description}
                                label={'Description'}
                                mandatory={true}
                                width="250px"
                                error={
                                  props.errors?.[`[${index}].description`]
                                    ? true
                                    : props.errors?.[`[${index}].bill_details`]
                                      ? true
                                      : false
                                }
                              />
                            </div>
                            <div>
                              <Select
                                width="250px"
                                name="expense_data_id"
                                mandatory={true}
                                label={'Site Expense'}
                                defaultLabel="Select the option"
                                placeholder="Select the option"
                                value={item.expense_data_id}
                                onChange={(e) => handleListChange(e, index)}
                                error={
                                  props.errors?.[`[${index}].expense_data_id`]
                                    ? true
                                    : props.errors?.[`[${index}].bill_details`]
                                      ? true
                                      : false
                                }
                              >
                                {getSiteExpense?.map((item: any, index: any) => {
                                  return (
                                    <option value={item.value}>{item.label}</option>
                                  );
                                })}
                              </Select>
                            </div>
                            <div>
                              <Select
                                name="bill_type"
                                value={item.bill_type}
                                width="250px"
                                label={'Bill Type'}
                                mandatory={true}
                                onChange={(e) => handleListChange(e, index)}
                                defaultLabel="Select the option"
                                placeholder="Select the option"
                                error={
                                  props.errors?.[`[${index}].bill_type`]
                                    ? true
                                    : props.errors?.[`[${index}].bill_details`]
                                      ? true
                                      : false
                                }
                              >
                                {options?.map((item, index) => {
                                  return (
                                    <option value={item.value}>{item.label}</option>
                                  );
                                })}
                              </Select>
                            </div>
                            <div>
                              <Input
                                name="bill_number"
                                mandatory={true}
                                width="250px"
                                label={'Bill Number'}
                                value={item.bill_number}
                                onChange={(e) => handleListChange(e, index)}
                                error={
                                  props.errors?.[`[${index}].bill_details`]
                                    ? true
                                    : false
                                }
                              />
                            </div>
                            <div style={{ overflow: 'hidden' }}>
                              <Input
                                name="total"
                                value={item.total}
                                onChange={(e) => handleListChange(e, index)}
                                width="250px"
                                label={'Amount'}
                                mandatory={true}
                                error={
                                  props.errors?.[`[${index}].total`]
                                    ? true
                                    : props.errors?.[`[${index}].bill_details`]
                                      ? true
                                      : false
                                }
                              />
                            </div>
                            <div>
                              {item.bill_details?.length > 0 &&
                                item.bill_details[0].is_delete === 'N' ? (
                                item.bill_details.map(
                                  (document: any, billIndex: number) => {
                                    if (document.is_delete === 'N')
                                      return (
                                        <div
                                          key={document.code}
                                          style={{
                                            width: '150px',
                                            cursor: 'pointer',
                                            fontWeight: 'bolder',
                                            color: 'blue',
                                            display: 'flex',
                                          }}
                                        >
                                          <div
                                            onClick={() => {
                                              viewDocumnet(item);
                                            }}
                                          >
                                            {customQuotationName}
                                          </div>
                                          <CloseIcon
                                            width={5}
                                            height={10}
                                            onClick={() => deleteFileinList(index)}
                                          />
                                        </div>
                                      );
                                  }
                                )
                              ) : (
                                <div>
                                  <div title="Attach document">
                                    <div style={{ paddingBottom: '10px' }}>
                                      <label>Document</label> <span style={{ color: 'red' }}>*</span>
                                    </div>
                                    <input
                                      ref={fileInputRef_2}
                                      id="upload-photo"
                                      name="upload_photo"
                                      type="file"
                                      style={{ display: 'none' }}
                                      onChange={(e) => handleFileSelectRow(e, index)}
                                      error={
                                        formik.touched.bill_number &&
                                        formik.errors.bill_number
                                      }
                                    />
                                    <div
                                      style={{
                                        cursor: 'pointer',
                                        paddingBottom: '5px',
                                      }}
                                      onClick={() => {
                                        setExpenseIndex(index);
                                        onButtonClickRow();
                                      }}
                                    >
                                      <FileUploadIcon color="#7f56d9" />
                                    </div>
                                    {fileMandatoryError && (
                                      <div className={Styles.documentErr}>
                                        {fileMandatoryError}
                                      </div>
                                    )}
                                  </div>
                                </div>
                              )}
                            </div>
                          </div>
                        </tr>
                      );
                    }
                  })}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      )}
      <CustomDelete
        open={openDelete}
        title="Delete Site Expense"
        contentLine1="Are you sure you want to delete this Expense ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteSiteExpense}
      />
      <Popup
        openPopup={openPdfpopup}
        setOpenPopup={setOpenPdfpopup}
        content={
          <SiteExpensesView
            openPopup={openPdfpopup}
            setOpenPopup={setOpenPdfpopup}
            viewDocs={viewDocs}
          />
        }
      />
    </div>
  );
};

export default SiteExpensesDetails;
