import React, { useRef, useState } from 'react';
import Styles from '../../../../styles/newStyles/projectSiteExpense.module.scss';
import Input from '../../../ui/Input';
import Select from '../../../ui/selectNew';
import DatePicker from '../../../ui/CustomDatePicker';
import Button from '../../../ui/Button';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import { getBymasertDataTypeDrop } from '../../../../hooks/masertData-hook';
import * as Yup from 'yup';
import { useFormik } from 'formik';
import Checkbox from '../../../ui/Checkbox';
import DeleteIcon from '../../../menu/icons/deleteIcon';
import CustomDelete from '../../../ui/customDeleteDialogBox';
import AddIcon from '../../../menu/icons/addIcon';
import TextArea from '../../../ui/CustomTextArea';
import NewAddCircleIcon from '../../../menu/icons/newAddCircleIcon';
import AttachmentIcon from '../../../menu/icons/attachementIcon';
import userService from '../../../../service/user-service';

const SiteExpensesDetails: React.FC = (props: any) => {
  console.log('props.expenseList', props.expenseList);

  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    expense_data_id: '',
    site_expense_name: '',
    bill_number: '',
    total: '',
    is_delete: false,
    description: '',
    bill_details: '',
  });
  const [ExpenseValue, setExpenseValue] = useState<any>({});
  const [checked, setChecked] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const { data: getSiteExpense } = getBymasertDataTypeDrop('SIEP');
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  console.log('selectedFileName', selectedFileName);

  const deleteSiteExpense = (e: any, values: any) => {
    console.log('ExpenseValue', ExpenseValue);
    const itemIndex = props.expenseList.findIndex(
      (item: any) =>
        item.expense_data_id === ExpenseValue?.expense_data_id &&
        item.is_delete === ExpenseValue?.is_delete
    );
    console.log('itemIndex', itemIndex);

    props.expenseList[itemIndex] = {
      ...props.expenseList[itemIndex],
      is_delete: true,
    };
    props.setExpenseList([...props.expenseList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
    props.setMessage('Site expanse details has been deleted');
    props.setOpenSnack(true);
  };
  const validationSchema = Yup.object().shape({
    is_delete: Yup.string().required(),
    expense_data_id: Yup.string()
      .typeError('Site Expense is required')
      .required('Site Expense is required')
      .test(
        'description-availability',
        'Site Expense is already present',
        async function (value, { parent }: Yup.TestContext) {
          const isDelete = false;
          console.log('isDelete', typeof isDelete);
          try {
            console.log('props.expenseList', props.expenseList);
            const isValuePresent = props.expenseList.some((obj) => {
              console.log('obj.is_delete ', typeof obj.is_delete);
              return (
                Number(obj.expense_data_id) === Number(value) &&
                obj.is_delete === isDelete
              );
            });
            console.log('isValuePresent', isValuePresent);

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
    total: Yup.number()
      .min(1, 'Amount must be more then 0')
      .max(100000, 'Amount must be less then 100000')
      .typeError('Amount is required')
      .required('Amount is required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      let code = 'SITEEXPENSE' + props.siteId;

      const s3UploadUrl: any = await handleDocuments(
        selectedFiles,
        code.toUpperCase()
      );

      console.log('s3UploadUrl', s3UploadUrl);

      console.log('s3UploadUrl', s3UploadUrl);
      formik.setFieldValue('bill_details', s3UploadUrl);
      console.log('valuesDetails', values);
      values['total'] = Number(values.total);
      values['bill_details'] = s3UploadUrl;
      props.setExpenseList([...props.expenseList, values]);
      setChecked(false);
      setSelectedFileName([]);
      resetForm();
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

  const handleFileSelect = async (e: any) => {
    const files = e.target.files;
    console.log('files', files);
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

        console.log('selectedFiles', selectedFilesArray);

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
  return (
    <div>
      <form>
        <div className={Styles.fields_container}></div>
      </form>
      <div className={Styles.table_container}>
        <table className={Styles.scrollable_table}>
          <thead>
            <th>#</th>
            <th>Site Expense</th>
            <th>Description</th>
            <th>Bill No</th>
            <th>Amount</th>
            <th>Document</th>
            <th>Action</th>
          </thead>
          <tbody>
            {props.expenseList?.length === 0 ? (
              <tr>
                <td colSpan="7" style={{ textAlign: 'center' }}>
                  No data found
                </td>
              </tr>
            ) : (
              ''
            )}
            {props.expenseList?.map((item: any, index: any) => {
              if (item.is_delete === false) {
                rowIndex = rowIndex + 1;
                const customQuotationName = generateCustomQuotationName(item);
                return (
                  <tr>
                    <td>{rowIndex}</td>
                    <td>
                      {item?.site_expense_name === undefined
                        ? item?.expense_master_data?.master_data_name
                        : item?.site_expense_name}
                    </td>
                    <td>{item.description}</td>
                    <td>{item.bill_number}</td>

                    <td>{item.total}</td>
                    <td>
                      {item.bill_details?.length > 0 ? (
                        item.bill_details.map(
                          (document: any, index: number) => (
                            <div key={document.code}>
                              <a
                                href={document.path}
                                target="_blank"
                                rel="noopener noreferrer"
                              >
                                {customQuotationName}
                                {/* Uploaded Document */}
                              </a>
                            </div>
                          )
                        )
                      ) : (
                        <div>-</div>
                      )}
                    </td>
                    <td>
                      <div
                        style={{ cursor: 'pointer' }}
                        onClick={() => {
                          setExpenseValue(item);
                          setOpenDelete(true);
                        }}
                      >
                        <DeleteIcon />
                      </div>
                    </td>
                  </tr>
                );
              }
            })}
            <tr>
              <td></td>
              <td>
                <div>
                  <AutoCompleteSelect
                    name="expense_data_id"
                    defaultLabel="Select from options"
                    placeholder="Select from options"
                    onChange={formik.handleChange}
                    value={formik.values.expense_data_id}
                    optionList={getSiteExpense}
                    onSelect={(value) => {
                      formik.setFieldValue('expense_data_id', value);
                      const matchingObjects = getSiteExpense.filter(
                        (obj: any) => Number(obj.value) === Number(value)
                      );
                      formik.setFieldValue(
                        'site_expense_name',
                        matchingObjects[0].label
                      );
                    }}
                    error={
                      formik.touched.expense_data_id &&
                      formik.errors.expense_data_id
                    }
                  />
                </div>
              </td>
              <td>
                <div>
                  <Input
                    name="description"
                    onChange={formik.handleChange}
                    value={formik.values.description}
                  />
                </div>
              </td>
              <td>
                <div style={{ paddingTop: '20px' }}>
                  <Input
                    name="bill_number"
                    value={formik.values.bill_number}
                    onChange={formik.handleChange}
                    disabled={checked === true ? false : true}
                  />
                  <div
                    style={{
                      display: 'flex',
                      gap: '10px',
                      alignItems: 'center',
                    }}
                  >
                    <Checkbox
                      checked={checked}
                      onChange={() => {
                        setChecked(!checked);
                      }}
                    />
                    <span style={{ fontSize: '70%' }}>Is bill available ?</span>
                  </div>
                </div>
              </td>

              <td>
                <div>
                  <Input
                    name="total"
                    value={formik.values.total}
                    onChange={formik.handleChange}
                    error={formik.touched.total && formik.errors.total}
                  />
                </div>
              </td>
              <td>
                <div title="Attach document">
                  <input
                    ref={fileInputRef}
                    id="upload-photo"
                    name="upload_photo"
                    type="file"
                    style={{ display: 'none' }}
                    onChange={handleFileSelect}
                  />
                  <div onClick={onButtonClick} style={{ cursor: 'pointer' }}>
                    <AttachmentIcon color="#7f56d9" />
                  </div>
                  <span>{selectedFileName[0]}</span>
                </div>
              </td>
              <td></td>
            </tr>
          </tbody>
        </table>
        <div className={Styles.addDataIcon}>
          <div onClick={formik.handleSubmit} className={Styles.iconContent}>
            <NewAddCircleIcon />
            <span>Add Plan here</span>
          </div>
        </div>
      </div>
      <CustomDelete
        open={openDelete}
        title="Delete Site Expense"
        contentLine1="Are you sure you want to delete this Expense ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteSiteExpense}
      />
    </div>
  );
};

export default SiteExpensesDetails;
