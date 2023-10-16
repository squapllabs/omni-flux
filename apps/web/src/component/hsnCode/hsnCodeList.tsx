import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/gstList.module.scss';
import {
  useDeleteHsnCode,
  uploadHsnCode,
  getByCode,
  useGetAllPaginatedHsnCodeData,
} from '../../hooks/hsnCode-hooks';
import CustomSnackBar from '../ui/customSnackBar';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import EditIcon from '../menu/icons/newEditIcon';
import CustomEditDialog from '../ui/customEditDialogBox';
import HsnForm from './hsnCodeCreate';
import Button from '../ui/Button';
import Button1 from '../menu/button';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import { createHsnCode } from '../../hooks/hsnCode-hooks';
import * as XLSX from 'xlsx';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/CustomPagination';
import CustomGroupButton from '../ui/CustomGroupButton';
import * as Yup from 'yup';
import { gethsnCreateValidateyup } from '../../helper/constants/hsn-constants';
import DownloadIcon from '../menu/icons/download';
import { store, RootState } from '../../redux/store';
import userService from '../../service/user-service';
import CloseIcon from '../menu/icons/closeIcon';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import TextArea from '../ui/CustomTextArea';
import CustomSidePopup from '../ui/CustomSidePopup';

const FileUploadValidationSchema = Yup.object().shape({
  file: Yup.mixed().required('Please upload a file'),
});
/* Function for HSN CODE */
const HsnCodeList = () => {
  const state: RootState = store.getState();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getByCode();
  const validationSchema = gethsnCreateValidateyup(Yup);
  const { mutate: getDeleteHsnCodeByID } = useDeleteHsnCode();
  const { mutate: uploadJsonData } = uploadHsnCode();
  const { mutate: createNewHsnCode } = createHsnCode();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [open, setOpen] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [hsnCodeId, setHsnCodeId] = useState();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [dataShow, setDataShow] = useState(false);
  const [openHsnForm, setOpenHsnForm] = useState(false);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [initialValues, setInitialValues] = useState({
    hsn_code_id: '',
    code: '',
    description: '',
  });
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [jsonData, setJsonData] = useState<{
    created_by: number;
    items: { code: string; description: string }[];
  } | null>(null);
  const [selectedFile, setSelectedFile] = useState<File | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [value, setValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const hsnCodeData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    global_search: filterValues.search_by_name,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedHsnCodeData(hsnCodeData);

  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  /* Function for deleting a HSN Code from the list */
  const deleteHscCode = () => {
    getDeleteHsnCodeByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleClose = () => {
    setOpenDelete(false);
  };
  /* Function for editing a HSN Code from the list */
  const editHscCodeHandler = (value: any) => {
    setMode('EDIT');
    setHsnCodeId(value);
    setOpenHsnForm(true);
  };

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      handleReset();
    }
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  /* Function for searching a HSN Code from the list */
  const handleSearch = async () => {
    const hsnData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(hsnData);
    setDataShow(true);
    setIsLoading(false);
    setFilter(true);
  };
  /* Function for resting the table to its actual state after search */
  const handleReset = async () => {
    setDataShow(false);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
    setIsResetDisabled(true);
  };

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          code: values.code,
          description: values.description,
        };
        createNewHsnCode(Object, {
          onSuccess: (data: { success: any }, variables: any, context: any) => {
            if (data?.success) {
              setMessage('HSN Code has successfully created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });
  /* Function for reading a excel file from system */
  const handleFileChange = async (e: any) => {
    const file = e.target.files[0];
    setSelectedFile(file);
    setError(null);
    if (file) {
      const reader = new FileReader();
      reader.onload = async function (e) {
        const data = e.target?.result as ArrayBuffer;
        if (data) {
          const workbook = XLSX.read(data, { type: 'binary' });
          const jsonData = await transformDatatoJson(workbook);
          setJsonData(jsonData);
        }
      };
      reader.readAsArrayBuffer(file);
    }
  };
  /* Function for converting the excel data into json format */
  const transformDatatoJson = async (workbook: any) => {
    const sheetName = workbook.SheetNames[0];
    const sheetData = XLSX.utils.sheet_to_json(workbook.Sheets[sheetName]);
    const userData = await userService.getOneUser(state.auth?.Data?.email);
    const created_by = userData?.data?.user_id;
    const jsonData = {
      created_by,
      items: sheetData.map((item: any) => ({
        code: item['code'],
        description: item['description'],
      })),
    };
    return jsonData;
  };
  /* Function for uploading the excel file */
  const handleUpload = () => {
    if (!selectedFile) {
      setError('Please select a file before uploading.');
      return;
    }
    FileUploadValidationSchema.validate({ file: selectedFile })
      .then(() => {
        if (jsonData) {
          uploadJsonData(jsonData, {
            onSuccess: (data, variables, context) => {
              if (data?.data?.successCount !== 0) {
                setMessage('Data uploaded successfully!');
                setOpenSnack(true);
                setError(null);
                setSelectedFile(null);
              } else {
                setError('No data to upload. Please select a valid file.');
              }
            },
          });
        } else {
          setError('No data to upload. Please select a valid file.');
        }
      })
      .catch((ValidationError) => {
        setError(ValidationError.message);
      });
  };

  const handleRemoveFile = () => {
    setSelectedFile(null);
    setError(null);
  };
  /* Function for converting json data into excel format */
  const convertToCSV = (data: any[]) => {
    const header = ['code', 'description'];
    const csvRows = [header.join(',')];
    for (const item of staticData) {
      const rowData = [item.code, item.description];
      csvRows.push(rowData.join(','));
    }
    return csvRows.join('\n');
  };

  const staticData = [
    {
      code: '1023',
      description: 'Sample Data - Details ',
    },
    {
      code: '1022',
      description: 'Sample Data 2 - Details',
    },
    {
      code: '1021',
      description: 'Sample Data 3 - Details',
    },
  ];
  /* Fuunction for downloading sample data */
  const handleDownload = () => {
    const csvContent = convertToCSV(staticData);
    const blob = new Blob([csvContent], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'data.csv';
    link.click();
    URL.revokeObjectURL(url);
  };

  const handleHsnFormClose = () => {
    setOpenHsnForm(false);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div>
      <div>
        <CustomLoader
          loading={searchLoader ? searchLoader : getAllLoadingPaginated}
          size={48}
          color="#333C44"
        >
          {initialData?.is_available ? (
            <div>
              <div className={Styles.topHeading}>
                <div className={Styles.heading}>
                  <div className={Styles.subHeading}>
                    <h3>HSN CODE</h3>
                  </div>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon color="white" />}
                      onClick={() => {
                        setMode('ADD');
                        setOpenHsnForm(true);
                      }}
                    >
                      Add HSN CODE
                    </Button>
                  </div>
                </div>
                <div className={Styles.filters}>
                  <div>
                    <Input
                      placeholder="Search Hsn Code"
                      width="300px"
                      prefixIcon={<SearchIcon />}
                      name="filter_value"
                      onChange={(e) => {
                        setFilterValues({
                          ...filterValues,
                          ['search_by_name']: e.target.value,
                        });
                        setCurrentPage(1)
                      }}
                    />
                  </div>
                  <div>
                    <CustomGroupButton
                      labels={buttonLabels}
                      onClick={handleGroupButtonClick}
                      activeButton={activeButton}
                    />
                  </div>
                </div>
              </div>
              <div className={Styles.uploads}>
                {/* <div style={{paddingTop:'5px'}}>
                    <h3>File Upload</h3>
                  </div> */}
                <div>
                  {selectedFile ? (
                    <div>
                      <span>{selectedFile.name}</span>
                      <button className={Styles.closeButton}>
                        <CloseIcon onClick={handleRemoveFile} />
                      </button>
                    </div>
                  ) : (
                    <button
                      className={Styles.fileSelect}
                      onClick={() => fileInputRef.current.click()}
                    >
                      Select File
                    </button>
                  )}
                  <input
                    type="file"
                    ref={fileInputRef}
                    className={Styles.input}
                    onChange={handleFileChange}
                  />
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    onClick={handleUpload}
                  >
                    Upload
                  </Button>
                </div>
                <div className={Styles.button}>
                  <Button1
                    text={
                      <div className={Styles.downloadButton}>
                        <DownloadIcon />
                        Download Sample Data
                      </div>
                    }
                    onClick={handleDownload}
                    backgroundColor="white"
                    textColor="black"
                    width={140}
                    border="1px solid #D0D5DD"
                    borderRadius={8}
                  />
                </div>
              </div>
              {error && <div className={Styles.error}>{error}</div>}

              <div className={Styles.box}>
                {/* <div className={Styles.textContent}>
                  <h3>List of HSN Code</h3>
                  <span className={Styles.content}>
                    Manage your HSN Code details here.
                  </span>
                </div>
                <div className={Styles.searchField}>
                  <div className={Styles.inputFilter}>
                    <Input
                      width="260px"
                      prefixIcon={<SearchIcon />}
                      name="search_by_name"
                      value={filterValues.search_by_name}
                      onChange={(e) => handleFilterChange(e)}
                      placeholder="Search"
                    />
                    <Button
                      className={Styles.searchButton}
                      shape="rectangle"
                      justify="center"
                      size="small"
                      onClick={handleSearch}
                    >
                      Search
                    </Button>
                    <Button
                      className={Styles.resetButton}
                      shape="rectangle"
                      justify="center"
                      size="small"
                      onClick={handleReset}
                      disabled={isResetDisabled}
                    >
                      Reset
                    </Button>
                  </div>
                  <div>
                    <CustomGroupButton
                      labels={buttonLabels}
                      onClick={handleGroupButtonClick}
                      activeButton={activeButton}
                    />
                  </div>
                </div> */}
                <div className={Styles.tableContainer}>
                  <div>
                    <table className={Styles.scrollable_table}>
                      <thead>
                        <tr>
                          <th>#</th>
                          <th>HSN Code</th>
                          <th>Description</th>
                          {activeButton === 'AC' && <th>Actions</th>}
                        </tr>
                      </thead>
                      <tbody>
                        {dataShow ? (
                          getFilterData?.total_count === 0 ? (
                            <tr>
                              <td></td>
                              <td></td>
                              <td>No data found</td>
                              {activeButton === 'AC' && <td></td>}
                            </tr>
                          ) : (
                            getFilterData?.content?.map(
                              (data: any, index: number) => (
                                <tr key={data.hsn_code_id}>
                                  <td>{startingIndex + index}</td>
                                  <td>{data.code}</td>
                                  <td>
                                    <span
                                      className={Styles.truncatedStyle}
                                      title={data.description}
                                    >
                                      {data.description
                                        ? data.description.length > 30
                                          ? data.description.substring(0, 30) +
                                          '...'
                                          : data.description
                                        : '-'}
                                    </span>
                                  </td>
                                  {activeButton === 'AC' && (
                                    <td>
                                      <div className={Styles.tablerow}>
                                        <EditIcon
                                          onClick={() =>
                                            editHscCodeHandler(data.hsn_code_id)
                                          }
                                        />
                                        <DeleteIcon
                                          onClick={() =>
                                            deleteCategoryHandler(data.hsn_code_id)
                                          }
                                        />
                                      </div>
                                    </td>
                                  )}
                                </tr>
                              )
                            )
                          )
                        ) : initialData?.total_count === 0 ? (
                          <tr>
                            <td></td>
                            <td></td>
                            <td>No data found</td>
                            {activeButton === 'AC' && <td></td>}
                          </tr>
                        ) : (
                          initialData?.content?.map((data: any, index: number) => (
                            <tr key={data.hsn_code_id}>
                              <td>{startingIndex + index}</td>
                              <td>{data.code}</td>
                              <td>
                                <span
                                  className={Styles.truncatedStyle}
                                  title={data.description}
                                >
                                  {data.description
                                    ? data.description.length > 30
                                      ? data.description.substring(0, 30) + '...'
                                      : data.description
                                    : '-'}
                                </span>
                              </td>
                              {activeButton === 'AC' && (
                                <td>
                                  <div className={Styles.tablerow}>
                                    <EditIcon
                                      onClick={() =>
                                        editHscCodeHandler(data.hsn_code_id)
                                      }
                                    />
                                    <DeleteIcon
                                      onClick={() =>
                                        deleteCategoryHandler(data.hsn_code_id)
                                      }
                                    />
                                  </div>
                                </td>
                              )}
                            </tr>
                          ))
                        )}
                      </tbody>
                    </table>
                  </div>
                </div>
                <div className={Styles.pagination}>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={
                      dataShow ? getFilterData?.total_page : initialData?.total_page
                    }
                    totalCount={
                      dataShow
                        ? getFilterData?.total_count
                        : initialData?.total_count
                    }
                    rowsPerPage={rowsPerPage}
                    onPageChange={handlePageChange}
                    onRowsPerPageChange={handleRowsPerPageChange}
                  />
                </div>
              </div>

            </div>
          ) : (
            <div>
              <div className={Styles.subHeading}>
              </div>
              <div className={Styles.emptyDataHandling}>
                <div>
                  <img
                    src="/HSN.png"
                    alt="aa"
                    width="100%"
                    height="250px"
                    style={{paddingBottom: '15px'}}
                  />
                </div>
                <div>
                  <h5>HSN code is Empty</h5>
                </div>
                <div>
                  <span className={Styles.spanContent}>Go ahead, add new HsnCode</span>
                </div>
                <div className={Styles.emptyButton}>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() => {
                      setMode('ADD');
                      setOpenHsnForm(true);
                    }}
                  >
                    Add HSN CODE
                  </Button>
                </div>
              </div>
            </div>
          )}
        </CustomLoader>
        <CustomDelete
          open={openDelete}
          title="Delete HSN Code"
          contentLine1="Are you sure you want to delete this HSN Code ?"
          contentLine2=""
          handleClose={handleClose}
          handleConfirm={deleteHscCode}
        />
        <CustomSidePopup
          open={openHsnForm}
          title={mode === 'EDIT' ? 'Edit HSN Code' : 'Add HSN Code'}
          handleClose={handleHsnFormClose}
          content={
            <HsnForm
              open={openHsnForm}
              setOpen={setOpenHsnForm}
              reload={reload}
              setReload={setReload}
              openSnack={openSnack}
              setOpenSnack={setOpenSnack}
              message={message}
              setMessage={setMessage}
              mode={mode}
              hsnCodeId={hsnCodeId}
            />
          }
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </div>
    </div>
  );
};

export default HsnCodeList;
