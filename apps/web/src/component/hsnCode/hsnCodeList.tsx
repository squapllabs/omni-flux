import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/newStyles/hsnList.module.scss';
import {
  useDeleteHsnCode,
  useUploadHsnCode,
  useGetAllPaginatedHsnCodeData,
} from '../../hooks/hsnCode-hooks';
import CustomSnackBar from '../ui/customSnackBar';
import EditIcon from '../menu/icons/newEditIcon';
import HsnForm from './hsnCodeCreate';
import Button from '../ui/Button';
// import Button1 from '../menu/button';
import Input from '../ui/Input';
import * as XLSX from 'xlsx';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/CustomPagination';
import * as Yup from 'yup';
import DownloadIcon from '../menu/icons/download';
import { store, RootState } from '../../redux/store';
import userService from '../../service/user-service';
import CloseIcon from '../menu/icons/closeIcon';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSidePopup from '../ui/CustomSidePopup';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import { handleSortByColumn } from './../../helper/common-function';

const FileUploadValidationSchema = Yup.object().shape({
  file: Yup.mixed().required('Please upload a file'),
});

/* Function for HSN CODE */
const HsnCodeList = () => {
  const state: RootState = store.getState();
  const { mutate: getDeleteHsnCodeByID } = useDeleteHsnCode();
  const { mutate: uploadJsonData } = useUploadHsnCode();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [hsnCodeId, setHsnCodeId] = useState();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [openHsnForm, setOpenHsnForm] = useState(false);
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
  const [sortColumn, setSortColumn] = useState('');
  const [sortOrder, setSortOrder] = useState('desc');
  const [modalOpen, setModalOpen] = useState(false);

  const hsnCodeData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: sortColumn === '' ? 'created_date' : sortColumn,
    order_by_direction: sortOrder,
    status: activeButton,
    global_search: filterValues.search_by_name,
  };
  /* Function to get all hsn code */
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedHsnCodeData(hsnCodeData);

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
    setModalOpen(true);
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton, sortColumn, sortOrder]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

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
    setModalOpen(false);
  };

  useEffect(() => {
    if (modalOpen === true) {
      document.body.style.overflow = 'hidden';
    } else {
      document.body.style.overflow = 'auto';
    }
  }, [modalOpen]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div>
      <div>
        <CustomLoader
          loading={getAllLoadingPaginated}
          size={48}
          color="#333C44"
        >
          {initialData?.is_available ? (
            <div>
              <div className={Styles.topHeading}>
                <div className={Styles.heading}>
                  <div className={Styles.subHeading}>
                    <h3>HSN Code</h3>
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
                        setModalOpen(true);
                      }}
                    >
                      Add HSN Code
                    </Button>
                  </div>
                </div>
                <div className={Styles.filters}>
                  <div className={Styles.searchFeild}>
                    <Input
                      placeholder="Search Hsn Code"
                      width="300px"
                      prefixIcon={<SearchIcon />}
                      name="filter_value"
                      onChange={(e) => {
                        setFilterValues({
                          ...filterValues,
                          search_by_name: e.target.value,
                        });
                        setCurrentPage(1);
                      }}
                    />
                  </div>
                </div>
              </div>
              <div className={Styles.uploads}>
                <div>
                  {selectedFile ? (
                    <div>
                      <span>{selectedFile.name}</span>
                      <button className={Styles.closeButton}>
                        <CloseIcon onClick={handleRemoveFile} />
                      </button>
                    </div>
                  ) : (
                    <Button
                      color="outlined"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      onClick={() => fileInputRef.current.click()}
                    >
                      Select File
                    </Button>
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
                  <Button
                    color="outlined"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<DownloadIcon color='#7f56d9'/>}
                    onClick={handleDownload}
                  >
                    Download Sample Data
                  </Button>
                  {/* <Button1
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
                  /> */}
                </div>
              </div>
              {error && <div className={Styles.error}>{error}</div>}

              <div className={Styles.box}>
                <div className={Styles.tableContainer}>
                  <div>
                    <table className={Styles.scrollable_table}>
                      <thead>
                        <tr>
                          <th>#</th>
                          <th
                            onClick={() =>
                              handleSortByColumn(
                                'code',
                                sortOrder,
                                setSortOrder,
                                setSortColumn
                              )
                            }
                          >
                            <div className={Styles.headingRow}>
                              <div>HSN code</div>
                              <div>
                                <FilterOrderIcon />
                              </div>
                            </div>
                          </th>
                          <th>Description</th>
                          {activeButton === 'AC' && <th>Actions</th>}
                        </tr>
                      </thead>
                      <tbody>
                        {initialData?.total_count === 0 ? (
                          <tr>
                            <td></td>
                            <td></td>
                            <td>No data found</td>
                            {activeButton === 'AC' && <td></td>}
                          </tr>
                        ) : (
                          initialData?.content?.map(
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
                                    </div>
                                  </td>
                                )}
                              </tr>
                            )
                          )
                        )}
                      </tbody>
                    </table>
                  </div>
                </div>
                <div className={Styles.pagination}>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={initialData?.total_page}
                    totalCount={initialData?.total_count}
                    rowsPerPage={rowsPerPage}
                    onPageChange={handlePageChange}
                    onRowsPerPageChange={handleRowsPerPageChange}
                  />
                </div>
              </div>
            </div>
          ) : (
            <div>
              <div className={Styles.subHeading}></div>
              <div className={Styles.emptyDataHandling}>
                <div>
                  <img
                    src="/HSN.png"
                    alt="aa"
                    width="100%"
                    height="200px"
                    style={{ paddingBottom: '15px' }}
                  />
                </div>
                <div>
                  <h5>HSN code is Empty</h5>
                </div>
                <div className={Styles.contentGap}>
                  <span className={Styles.spanContent}>
                    Go ahead, add new HsnCode
                  </span>
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
              setModalOpen={setModalOpen}
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
