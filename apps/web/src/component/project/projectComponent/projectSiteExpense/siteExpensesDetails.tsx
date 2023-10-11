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
import DeleteIcon from '../../../menu/icons/newDeleteIcon';
import CustomDelete from '../../../ui/customDeleteDialogBox';
import AddIcon from '../../../menu/icons/addIcon';
import TextArea from '../../../ui/CustomTextArea';
import NewAddCircleIcon from '../../../menu/icons/newAddCircleIcon';
import AttachmentIcon from '../../../menu/icons/attachementIcon';
import FileUploadIcon from '../../../menu/icons/fileUploadIcon';
import userService from '../../../../service/user-service';
import Popup from '../../../ui/CustomPdfPopup';
import SiteExpensesView from './siteExpensesView';
import ViewIcon from '../../../menu/icons/newViewIcon';
import MoneyIcon from '../../../menu/icons/MoneyIcon';
// import AddIcon from '../../../menu/icons/addIcon';

const SiteExpensesDetails: React.FC = (props: any) => {
  // console.log('props.expenseList', props.expenseList);

  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    expense_data_id: '',
    site_expense_name: '',
    bill_number: '',
    total: '',
    is_delete: false,
    description: '',
    bill_details: '',
    status: '',
  });
  const [openPopup, setOpenPopup] = useState(false);
  const [openPdfpopup, setOpenPdfpopup] = useState(false);
  const [viewDocs, setViewDocs] = useState();
  const [ExpenseValue, setExpenseValue] = useState<any>({});
  const [checked, setChecked] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [fileMandatoryError, setFileMandatoryError] = useState('');
  const [viewAddRow, setViewAddRow] = useState(false);
  const [viewAddRowButton, setViewAddRowButton] = useState(true);
  const { data: getSiteExpense } = getBymasertDataTypeDrop('SIEP');
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  // console.log('selectedFileName', selectedFileName);

  const deleteSiteExpense = (e: any, values: any) => {
    // console.log('ExpenseValue', ExpenseValue);
    const itemIndex = props.expenseList.findIndex(
      (item: any) =>
        item.expense_data_id === ExpenseValue?.expense_data_id &&
        item.is_delete === ExpenseValue?.is_delete
    );
    // console.log('itemIndex', itemIndex);

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
          // console.log('isDelete', typeof isDelete);
          try {
            // console.log('props.expenseList', props.expenseList);
            const isValuePresent = props.expenseList.some((obj) => {
              // console.log('obj.is_delete ', typeof obj.is_delete);
              return (
                Number(obj.expense_data_id) === Number(value) &&
                obj.is_delete === isDelete
              );
            });
            // console.log('isValuePresent', isValuePresent);
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
      .typeError('Only Numbers are allowed')
      .required('Amount is required'),
    bill_number: Yup.string()
      // .required('bill number is required')
      .test(
        'document check',
        'Site document mandatory ',
        async function (value, { parent }: Yup.TestContext) {
          try {
            if (value && selectedFiles.length === 0) {
              setFileMandatoryError(
                'File is mandatory when a bill number is provided'
              );
              return false;
            } else {
              setFileMandatoryError('');
              return true;
            }
          } catch {
            return true;
          }
        }
      ),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
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
      setChecked(false);
      setSelectedFileName([]);
      resetForm();
      setSelectedFiles([]);
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

  const viewDocumnet = (value: any) => {
    setOpenPdfpopup(true);
    setViewDocs(value);
  };

  const handleFileSelect = async (e: any) => {
    const files = e.target.files;
    // console.log('files', files);
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
        // console.log('selectedFiles', selectedFilesArray);
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
  const handleAddRowButton = () => {
    setViewAddRow(true);
    setViewAddRowButton(false);
  };
  return (
    <div>
      <form>
        <div className={Styles.fields_container}></div>
      </form>
      <div className={Styles.table_container}>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th>#</th>
              <th>Description</th>
              <th>Site Expense</th>
              <th>Bill No</th>
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
                    <td>{item.description}</td>
                    <td>
                      {item?.site_expense_name === undefined
                        ? item?.expense_master_data?.master_data_name
                        : item?.site_expense_name}
                    </td>
                    <td>{item.bill_number}</td>
                    <td>{item.total}</td>
                    <td>
                      {item.bill_details?.length > 0 ? (
                        item.bill_details.map(
                          (document: any, index: number) => (
                            <div key={document.code} style={{ width: '150px' }}>
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
                      <div className={Styles.buttons}>
                        <div
                          style={{ cursor: 'pointer' }}
                          onClick={() => {
                            setExpenseValue(item);
                            setOpenDelete(true);
                          }}
                        >
                          <DeleteIcon />
                        </div>
                        <div
                          style={{ cursor: 'pointer' }}
                          onClick={() => {
                            viewDocumnet(item);
                          }}
                        >
                          <ViewIcon />
                        </div>
                      </div>
                    </td>
                  </tr>
                );
              }
            })}
            {viewAddRow || props?.mode === 'Edit' ? (
              <tr>
                <td></td>
                <td>
                  <div>
                    <Input
                      name="description"
                      onChange={formik.handleChange}
                      value={formik.values.description}
                      width="300px"
                    />
                  </div>
                </td>
                <td>
                  <div>
                    <AutoCompleteSelect
                      name="expense_data_id"
                      defaultLabel="Select from options"
                      placeholder="Select from options"
                      onChange={formik.handleChange}
                      value={formik.values.expense_data_id}
                      optionList={getSiteExpense}
                      width="180px"
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
                <td style={{ overflow: 'hidden' }}>
                  <div>
                    <Input
                      name="bill_number"
                      value={formik.values.bill_number}
                      onChange={formik.handleChange}
                      width="120px"
                    />
                  </div>
                </td>
                <td style={{ overflow: 'hidden' }}>
                  <div>
                    <Input
                      name="total"
                      value={formik.values.total}
                      onChange={formik.handleChange}
                      error={formik.touched.total && formik.errors.total}
                      width="120px"
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
                      error={
                        formik.touched.bill_number && formik.errors.bill_number
                      }
                    />
                    <div style={{ cursor: 'pointer', paddingBottom: '5px' }}>
                      <FileUploadIcon onClick={onButtonClick} color="#7f56d9" />
                    </div>
                    <div><span>{selectedFileName[0]}</span></div>
                    {fileMandatoryError && (
                      <div className={Styles.documentErr}>
                        {fileMandatoryError}
                      </div>
                    )}
                  </div>
                </td>
                <td></td>
              </tr>
            ) : (
              ''
            )}
          </tbody>
        </table>
        {viewAddRow || props?.mode === 'Edit' ? (
          <div className={Styles.addDataIcon}>
            <div onClick={formik.handleSubmit} className={Styles.iconContent}>
              <NewAddCircleIcon />
              <span>Add Expenses</span>
            </div>
          </div>
        ) : (
          ''
        )}
        {props?.mode !== 'Edit' && viewAddRowButton ? (
          <div className={Styles.addNewRowView}>
            <MoneyIcon height={50} width={50} color="#475467" />
            <h5>No Site Expenses added for this site </h5>
            <span className={Styles.spanContent}>Let's add an expanse now</span>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              icon={<AddIcon color="white" />}
              onClick={() => handleAddRowButton()}
            >
              Add Expense
            </Button>
          </div>
        ) : (
          ''
        )}
      </div>
      <CustomDelete
        open={openDelete}
        title="Delete Site Expense"
        contentLine1="Are you sure you want to delete this Expense ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteSiteExpense}
      />
      <Popup
        // title="Pdf Viewer"
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
