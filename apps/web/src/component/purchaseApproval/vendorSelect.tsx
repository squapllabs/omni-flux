import React, { useState, useEffect, useRef } from 'react';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Button from '../ui/Button';
import CustomLoader from '../ui/customLoader';
import { environment } from '../../environment/environment';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate, useLocation } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import Styles from '../../styles/vendorSelect.module.scss';
import CustomEditDialog from '../ui/customEditDialogBox';
import vendorQuotesService from '../../service/vendorQuotes-service';
import PurchaseRequestEdit from './purchaseRequestEdit';
import {
  updateVendorQuotes,
  getByPRbasedVendorQuotes,
  getVendorDetailsBasedONPR,
  getVendorquotesBYventorquotesID,
} from '../../hooks/vendorQuotes-hooks';
import BackArrowIcon from '../menu/icons/backArrow';
import CustomMenu from '../ui/CustomMenu';
import CustomSnackBar from '../ui/customSnackBar';
import { Link } from 'react-router-dom';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import ProjectSubheader from '../project/projectSubheader';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Select from '../ui/selectNew';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import FileUploadIcon from '../menu/icons/fileUploadIcon';
import userService from '../../service/user-service';
import CloseIcon from '../menu/icons/closeIcon';
import TextArea from '../ui/CustomTextArea';
import PageDisabled from '../ui/pageDisableComponent';

const VendorSelect = () => {
  const routeParams = useParams();
  const location = useLocation();
  const indentId = location.state.indent_id;
  const projectId = location.state.project_id;
  console.log('indentId', indentId);
  console.log('indentId', indentId);

  const navigate = useNavigate();
  const { mutate: updateOneVendorQuotes } = updateVendorQuotes();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const prID = Number(routeParams?.id);
  const { data: getVendorList = [], isLoading: vendorLoading } =
    getVendorDetailsBasedONPR(prID);
  console.log('getVendorList', getVendorList);

  // console.log('initialSiteId', initialSiteId);

  const [tableData, setTableData] = useState<any>([]);
  const [totalBudget, setTotalBudget] = useState<any>();
  const [Id, setID] = useState();
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [reload, setReload] = useState(false);
  const [isAnyRowApproved, setIsAnyRowApproved] = useState(false);
  const [initialValues, setInitialValues] = useState({
    vendor_quotes_id: '',
    total_quotation_amount: '',
    purchase_request_id: '',
    vendor_id: '',
    quotation_status: '',
    remarks: '',
    vendor_quotes_documents: [],
  });
  const [vendorQuoteID, setVendorQuoteID] = useState<any>();
  const [vendorQuoteDocument, setVendorQuoteDocument] = useState<any>([]);
  const [vendorQuoteData, setVendorQuoteData] = useState<any>({});
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [pageDisable, setPageDisabale] = useState(false);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [fileMandatoryError, setFileMandatoryError] = useState('');
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };
  useEffect(() => {
    const fetchData = async () => {
      console.log('vendorQuoteID', vendorQuoteID);
      const vendorQuotesDetails =
        await vendorQuotesService.getVendorquotesBYventorquotesID(
          vendorQuoteID
        );
      const data = await vendorQuotesService.getOneVendorQuotesById(
        vendorQuoteID
      );
      setInitialValues({
        vendor_quotes_id: data?.data?.vendor_quotes_id,
        purchase_request_id: data?.data?.purchase_request_id,
        vendor_id: data?.data?.vendor_id,
        quotation_status: data?.data?.quotation_status,
        total_quotation_amount: data?.data?.total_quotation_amount,
        remarks: data?.data?.remarks,
        vendor_quotes_documents: data?.data?.vendor_quotes_documents,
      });
      setVendorQuoteData(data?.data);
      setVendorQuoteDocument(data?.data?.vendor_quotes_documents);
      setTableData(vendorQuotesDetails?.data);
    };
    fetchData();
  }, [vendorQuoteID]);
  useEffect(() => {
    if (tableData?.length > 0) {
      const sumOfRates = tableData.reduce(
        (accumulator: any, currentItem: any) => {
          return accumulator + currentItem.total_cost;
        },
        0
      );
      formik.setFieldValue('total_quotation_amount', sumOfRates);
      setTotalBudget(sumOfRates);
    }
  }, [tableData]);
  console.log('tableData', tableData);

  const vendorData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'asc',
    status: 'AC',
    global_search: '',
    purchase_request_id: prID,
  };
  const {
    data: getVendorQuotes,
    isLoading: loading,
    refetch,
  } = getByPRbasedVendorQuotes(vendorData);
  console.log('getVendorQuotes', getVendorQuotes);
  useEffect(() => {
    const initialSiteId =
      vendorLoading == false ? getVendorList[0]?.value : null;

    const quotesData = getVendorQuotes?.content?.filter(
      (obj: any) => obj.quotation_status === 'Approved'
    );
    if (quotesData != undefined) {
      const is_available = isAvilable();
      setPageDisabale(is_available);
      if (quotesData?.length > 0) {
        setVendorQuoteID(quotesData[0].vendor_quotes_id);
      } else {
        setVendorQuoteID(initialSiteId);
      }
    }
    refetch();
    // setVendorQuoteID(initialSiteId);
  }, [!vendorLoading && !loading]);
  const generateCustomQuotationName = (data: any) => {
    if (data) {
      const vendorName =
        data?.vendor_name === undefined
          ? data?.vendor_data?.vendor_name
          : data?.vendor_name;
      const year = new Date().getFullYear();
      const customBillName = `ALM-${vendorName.substring(0, 5)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };
  const handleEdit = (value: any) => {
    setMode('EDIT');
    setID(value);
    setOpen(true);
    setReload(false);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleApprove = (value: any) => {
    setID(value);
    handleSubmit(value);
  };
  const handleVendourQuotes = (event: any, index: number) => {
    let tempObj = {};
    tempObj = {
      ...tableData[index],
      [event.target.name]: Number(event.target.value),
      ['total_cost']:
        tableData[index].purchase_requested_quantity *
        Number(event.target.value),
    };
    let tempArry = [...tableData];
    tempArry[index] = tempObj;
    setTableData(tempArry);
  };

  const handleSubmit = async (id: any) => {
    try {
      const data = await vendorQuotesService.getOneVendorQuotesById(id);
      const obj = {
        vendor_quotes_id: data?.data?.vendor_quotes_id,
        purchase_request_id: data?.data?.purchase_request_id,
        vendor_id: data?.data?.vendor_id,
        quotation_status: 'Approved',
        updated_by: userID,
        vendor_quotes_documents: data?.data?.vendor_quotes_documents,
        total_quotation_amount: data?.data?.total_quotation_amount,
      };
      updateOneVendorQuotes(obj, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Vendor Approved');
            setOpenSnack(true);
            navigate(`/purchase-request-list/${indentId}`, {
              state: { project_id: projectId },
            });
          }
        },
      });
    } catch {
      console.log('Error occured in vendor select ');
    }
  };
  const isAvilable = () => {
    return getVendorQuotes?.content?.some(
      (obj) => obj.quotation_status === 'Approved'
    );
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  const handleSubmitQuotation = () => {
    formik.submitForm();
  };
  const validationSchema = Yup.object().shape({
    vendor_id: Yup.string().required('Please select Vendor'),
    remarks: Yup.string().required('remarks required'),
    vendor_quotes_documents: Yup.array()
      .required()
      .test(
        'description-availability',
        'Site Expense is already present',
        async function (value, { parent }: Yup.TestContext) {
          let bill_details = parent.vendor_quotes_documents;
          console.log('bill_details', bill_details);
          console.log('bill_detailslenght', bill_details.length);
          if (bill_details?.length < 0 && bill_details[0]?.is_delete === 'Y') {
            return true;
          } else if (
            bill_details?.length > 0 &&
            bill_details[0]?.is_delete === 'N'
          ) {
            return true;
          } else {
            console.log('open');
            setMessage('Bill is Missing');
            setOpenSnack(true);
            return false;
          }
        }
      ),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      console.log('values', values);
      const obj = {
        vendor_quotes_id: values.vendor_quotes_id,
        purchase_request_id: prID,
        vendor_id: values.vendor_id,
        quotation_status: 'Quotation Recived',
        updated_by: userID,
        vendor_quotes_documents: vendorQuoteDocument,
        total_quotation_amount: values.total_quotation_amount,
        vendor_quotation_details: tableData,
        remarks: values?.remarks,
      };
      updateOneVendorQuotes(obj, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Vendor Approved');
            setOpenSnack(true);
            refetch();
            // navigate(`/purchase-request-list/${indentId}`, {
            //   state: { project_id: projectId },
            // });
          }
        },
      });
    },
  });
  const handleFileSelect = async (e: any) => {
    const files = e.target.files;
    if (files.length > 0) {
      const fileList: File[] = Array.from(files);
      const oversizedFiles = fileList.filter(
        (file) => file.size > 10 * 1024 * 1024
      );
      if (oversizedFiles.length > 0) {
        const oversizedFileNames = oversizedFiles
          .map((file) => file.name)
          .join(', ');
        // props.setLoader(!props.loader);
        const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
        setFileSizeError(errorMessage);
        setMessage(errorMessage);
        // props.setOpenSnack(true);
      } else {
        const selectedFilesArray: File[] = [];
        const selectedFileNamesArray: string[] = [];
        let arr: any = [];
        fileList.forEach(async (file) => {
          const code = 'PR' + prID;
          const response = await userService.documentUpload(file, code);
          console.log('response', response?.data[0]);

          let obj = {
            ...response?.data[0],
            is_delete: 'N',
          };
          console.log('responseobj', obj);
          arr.push(obj);
          selectedFilesArray.push(file);
          selectedFileNamesArray.push(file.name);
        });
        setVendorQuoteDocument(arr);
        formik.setFieldValue('vendor_quotes_documents', arr);
        setSelectedFiles(selectedFilesArray);
        setSelectedFileName(selectedFileNamesArray);
        setFileSizeError('');
        // props.setLoader(false);
        setMessage('Document uploaded');
        setOpenSnack(true);
      }
    }
  };
  console.log('formik.values.vendor_quotes_id', formik.values.vendor_quotes_id);

  const deleteFileinList = (index: any) => {
    let tempObj = {};
    vendorQuoteDocument[index].is_delete = 'Y';
    tempObj = {
      ...vendorQuoteDocument[index],
    };
    let tempArry = [...vendorQuoteDocument];
    tempArry[index] = tempObj;
    setVendorQuoteDocument(tempArry);
  };
  return (
    <div className={Styles.container}>
      <CustomLoader loading={loading} size={48} color="#333C44">
        {/* <div className={Styles.textContent}>
          <h3>Vendor Detail List</h3>
          <span className={Styles.content}>Select the apt vendor</span>
        </div> */}
        <div>
          <ProjectSubheader
            title="Allocated Vendor Detail List"
            description="Select the apt vendor"
            navigation={`/purchase-request-list/${indentId}`}
          />
        </div>
        {/* <div className={Styles.dividerStyle}></div> */}
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S No</th>
                {/* <th>Vendor ID</th> */}
                <th>Vendor Name </th>
                <th>No of Items</th>
                <th>Quotation Budget</th>
                <th>Quotation Id</th>
                <th>Quotation Status</th>
                <th>Document</th>
                <th>Options</th>
              </tr>
            </thead>
            <tbody>
              {getVendorQuotes?.content?.map((data: any, index: number) => {
                const customQuotationName = generateCustomQuotationName(data);
                const itemData = data?.quotation_details;
                const isQuotationPending = data.quotation_status === 'Pending';
                const isQuotationRecived =
                  data.quotation_status === 'Quotation Recived';
                const isAvai = isAvilable();
                console.log('isAvai', isAvai);
                if (data.quotation_status === 'Approved') {
                  console.log('dataApproved', data?.vendor_quotes_id);
                }
                const actions = [
                  {
                    label: 'Edit',
                    onClick: () => {
                      if (!isAnyRowApproved) {
                        handleEdit(data.vendor_quotes_id);
                      }
                    },
                    disabled: isAnyRowApproved,
                  },
                  {
                    label: 'Approve',
                    onClick: () => {
                      if (!isQuotationPending && !isAnyRowApproved) {
                        handleApprove(data.vendor_quotes_id);
                      }
                    },
                    disabled: isQuotationPending || isAnyRowApproved,
                  },
                ];
                const isAnyActionEnabled = actions.some(
                  (action) => !action.disabled
                );
                return (
                  <tr key={data.vendor_quotes_id}>
                    <td>{startingIndex + index}</td>
                    {/* <td>{data.vendor_id || nullLableNameFromEnv}</td> */}
                    <td>{data.vendor_name || nullLableNameFromEnv}</td>
                    <td>
                      {data?.quotation_details?.length || nullLableNameFromEnv}
                    </td>
                    <td>
                      {formatBudgetValue(data.total_quotation_amount) ||
                        nullLableNameFromEnv}
                    </td>
                    <td>{data.quotation_id}</td>
                    <td>{data?.quotation_status || nullLableNameFromEnv}</td>
                    <td>
                      {data?.vendor_quotes_documents?.length === 0 ||
                      data?.vendor_quotes_documents === null
                        ? nullLableNameFromEnv
                        : data?.vendor_quotes_documents?.map(
                            (document: any, index: any) => (
                              <ol key={index}>
                                <a
                                  href={document.path}
                                  target="_blank"
                                  rel="noopener noreferrer"
                                >
                                  {customQuotationName}
                                </a>
                              </ol>
                            )
                          )}
                    </td>
                    <td>
                      {/* {isAnyActionEnabled && <CustomMenu actions={actions} />} */}
                      <div
                        style={{
                          pointerEvents:
                            isQuotationRecived && isAvai != true ? '' : 'none',
                        }}
                      >
                        <div
                          onClick={() => {
                            if (isQuotationRecived && isAvai != true) {
                              handleApprove(data.vendor_quotes_id);
                            }
                          }}
                          className={`${Styles.status} ${
                            data?.quotation_status === 'Approved'
                              ? Styles.completedStatus
                              : data?.quotation_status === 'Quotation Recived'
                              ? Styles.inprogressStatus
                              : ''
                          }`}
                        >
                          {data?.quotation_status === 'Approved'
                            ? 'Approved'
                            : isAvai != true && isQuotationRecived
                            ? 'Approve'
                            : '--'}
                          {/* {isAvai != true && isQuotationRecived
                            ? 'Approve'
                            : '--'} */}
                        </div>
                      </div>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
        <div className={Styles.dividerStyle}></div>
        <PageDisabled disabled={pageDisable}>
          {' '}
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div>
                <AutoCompleteSelect
                  name="vendor"
                  label="Select Vendor"
                  defaultLabel="Select from vendor"
                  placeholder="Select from vendor"
                  // value={intialVendor}
                  value={
                    formik.values.vendor_quotes_id === undefined
                      ? Number(vendorQuoteID)
                      : formik.values.vendor_quotes_id
                  }
                  onChange={formik.handleChange}
                  optionList={getVendorList != null ? getVendorList : []}
                  onSelect={(value) => {
                    console.log('value', value);
                    setVendorQuoteID(Number(value));
                    const matchingObjects = getVendorList.filter(
                      (obj: any) => Number(obj.value) === Number(value)
                    );
                    console.log('matchingObjects', matchingObjects[0]);
                    formik.setFieldValue('vendor_quotes_id', value);
                    formik.setFieldValue(
                      'vendor_id',
                      matchingObjects[0]?.data?.vendor_id
                    );
                  }}
                  error={formik.touched.vendor_id && formik.errors.vendor_id}
                  disabled={pageDisable}
                />
              </div>
              <div
                style={{
                  display: 'flex',
                  flexDirection: 'column',
                  gap: '10px',
                }}
              >
                <div className={Styles.uploadLabel}>Upload Quotation</div>
                {vendorQuoteDocument?.length > 0 &&
                vendorQuoteDocument[0].is_delete === 'N' ? (
                  <div>
                    {vendorQuoteDocument?.map((document: any, index: any) => {
                      const customQuotationName =
                        generateCustomQuotationName(vendorQuoteData);

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
                              fontSize: '15px',
                            }}
                          >
                            <div
                            // onClick={() => {
                            //   viewDocumnet(item);
                            // }}
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
                    })}
                  </div>
                ) : (
                  <div title="Attach document">
                    <input
                      ref={fileInputRef}
                      id="upload-photo"
                      name="upload_photo"
                      type="file"
                      style={{ display: 'none' }}
                      onChange={(e) => handleFileSelect(e)}
                    />
                    <div
                      style={{
                        cursor: 'pointer',
                        paddingBottom: '5px',
                      }}
                      onClick={() => {
                        onButtonClick();
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
                )}
              </div>
            </div>
            <div style={{ display: 'flex', justifyContent: 'flex-end' }}>
              <div className={Styles.horizontalLine}></div>
              <div>
                <h3>{formatBudgetValue(totalBudget ? totalBudget : 0)}</h3>
                <p className={Styles.countContentTitle}>Quotation Budget</p>
              </div>
            </div>
          </div>
          <div>
            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>S No</th>
                    <th>Items </th>
                    <th>Indent Requested Quantity</th>
                    <th>Purchase Requested Quantity</th>
                    <th>Quotation Cost</th>
                    <th>Total Cost</th>
                  </tr>
                </thead>
                <tbody>
                  {tableData?.map((items: any, index: number) => {
                    return (
                      <tr>
                        <td>{index + 1}</td>
                        <td>{items?.item_data?.item_name}</td>
                        <td>{items?.indent_requested_quantity}</td>
                        <td>{items?.purchase_requested_quantity}</td>
                        <td>
                          <Input
                            name="unit_cost"
                            value={items?.unit_cost}
                            error={false}
                            width="100px"
                            onChange={(e) => handleVendourQuotes(e, index)}
                          />
                        </td>
                        <td>{items?.total_cost}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
          <div
            // style={{
            //   display: 'flex',
            //   padding: '10px',
            //   justifyContent: 'space-between',
            // }}
            className={Styles.searchField}
          >
            <div>
              <TextArea
                name="remarks"
                label="Remarks"
                rows={5}
                maxCharacterCount={1000}
                value={formik.values.remarks ? formik.values.remarks : ''}
                onChange={formik.handleChange}
                width="500px"
                error={formik.touched.remarks && formik.errors.remarks}
              />
            </div>
            <div
              style={{
                display: 'flex',
                alignItems: 'end',
                paddingBottom: '20px',
              }}
            >
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                onClick={formik.handleSubmit}
              >
                Submit Quotation
              </Button>
            </div>
          </div>
        </PageDisabled>

        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomEditDialog
          open={open}
          content={
            <PurchaseRequestEdit
              setOpen={setOpen}
              open={open}
              mode={mode}
              setReload={setReload}
              vendorID={Id}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
      </CustomLoader>
    </div>
  );
};
export default VendorSelect;
