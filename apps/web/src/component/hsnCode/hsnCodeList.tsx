import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/gstList.module.scss';
import { useGetAllHsnCode, useDeleteHsnCode, uploadHsnCode, getByCode } from '../../hooks/hsnCode-hooks';
import CustomDialog from '../ui/customDialog';
import MySnackbar from '../ui/MySnackbar';
import DeleteIcon from '../menu/icons/deleteIcon';
import EditIcon from '../menu/icons/editIcon';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import HsnForm from './hsnCodeCreate';
import Button from '../ui/Button';
import Button1 from '../menu/button';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import { createHsnCode } from '../../hooks/hsnCode-hooks';
import * as XLSX from "xlsx"
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomGroupButton from '../ui/CustomGroupButton';
import * as Yup from 'yup';
import { gethsnCreateValidateyup } from '../../helper/constants/hsn-constants';
import DownloadIcon from '../menu/icons/download';
import { store, RootState } from '../../redux/store';
import userService from '../../service/user-service';
import CloseIcon from '../menu/icons/closeIcon';

const FileUploadValidationSchema = Yup.object().shape({
  file: Yup.mixed().required('Please upload a file'),
});

const HsnCodeList = () => {
  const state: RootState = store.getState();
  const { isLoading: getAllLoading } = useGetAllHsnCode();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
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
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IC' },
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
  const [jsonData, setJsonData] = useState<{ created_by: number; items: { code: string; description: string }[] } | null>(null);
  const [selectedFile, setSelectedFile] = useState<File | null>(null);
  const [error, setError] = useState<string | null>(null);

  const deleteHscCode = ( value: any) => {
    getDeleteHsnCodeByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleClose = () => {
    setOpen(false);
  };

  const editHscCodeHandler = ( value: any) => {
    setMode('EDIT');
    setHsnCodeId(value);
    setOpen(true);
  };

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  const handleSearch = async () => {
    const demo: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(true);
  };

  const handleReset = async () => {
    const demo: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
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
          onSuccess: (data: { success: any; }, variables: any, context: any) => {
            if (data?.success) {
              setMessage('UOM has successfully created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });

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

  const transformDatatoJson = async (workbook: any) => {
    const sheetName = workbook.SheetNames[0];
    const sheetData = XLSX.utils.sheet_to_json(workbook.Sheets[sheetName]);
    const userData = await userService.getOneUser(state.auth?.Data?.email)
    const created_by = userData?.data?.userData?.user_id;
    const jsonData = {
      created_by,
      items: sheetData.map((item: any) => ({
        code: item['code'],
        description: item['description'],
      })),
    };
    return jsonData;
  }


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
              if (data) {
                setMessage('Data uploaded successfully!');
                setOpenSnack(true);
                setError(null)
                setSelectedFile(null);
              }
            },
          });
        }
        else {
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

  const convertToCSV = (data: any[]) => {
    const header = [
      'code',
      'description'
    ];
    const csvRows = [header.join(',')];
    for (const item of staticData) {
      const rowData = [
        item.code,
        item.description
      ];
      csvRows.push(rowData.join(','));
    }
    return csvRows.join('\n');
  }

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

  return (
    <div>
      <div>
        <CustomLoader
          loading={isLoading === true ? getAllLoading : FilterLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>Add New HSN Code</h3>
              <span className={Styles.content}>
                Manage your HSN details here.
              </span>
            </div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.fields}>
                <div>
                  <Input
                    label="Code"
                    placeholder="Enter HSN code"
                    name="code"
                    value={formik.values.code}
                    onChange={formik.handleChange}
                    error={formik.touched.code && formik.errors.code}
                    width="100%"
                  />
                </div>
                <div>
                  <Input
                    label="Description"
                    placeholder="Enter HSN Code description"
                    name="description"
                    value={formik.values.description}
                    onChange={formik.handleChange}
                    error={formik.touched.description && formik.errors.description}
                    width="100%"
                  />
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                  >
                    Add New HSN Code
                  </Button>
                </div>
              </div>
            </form>
            <div className={Styles.uploads}>
              <div >
                {selectedFile ? (
                  <div>
                    <span>{selectedFile.name}</span>
                    <button
                      style={{ backgroundColor: 'white', marginTop: '8%', marginLeft: '5px' }}
                      ><CloseIcon onClick={handleRemoveFile}/></button>
                  </div>
                ) : (
                  <button
                    style={{ padding: '10px' }}
                    onClick={() => fileInputRef.current.click()}>Select File</button>
                )}
                <input
                  type="file"
                  ref={fileInputRef}
                  style={{ display: 'none', }}
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
                      <DownloadIcon style={{ padding: '4px' ,width:'50px',paddingBottom:'15px' }}/>
                      Download sample Data
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
            {error && <div style={{ color: 'red', fontSize: 'small', padding: '5px' }}>{error}</div>}
          </div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
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
            </div>
            <div className={Styles.tableContainer}>
              <div>
                <table>
                  <thead>
                    <tr>
                      <th>HSN Code</th>
                      <th>Description</th>
                      <th>Options</th>
                    </tr>
                  </thead>
                  <tbody>
                    {getFilterData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td>No data found</td>
                        <td></td>
                      </tr>
                    ) : (
                      ''
                    )}
                    {getFilterData?.content?.map((data: any) => (
                      <tr >
                        <td>{data.code}</td>
                        <td>
                          <span className={Styles.truncatedStyle} title={data.description}>
                            {data.description.substring(0,20)}
                          </span>
                        </td>
                        <td>
                            
                            <EditIcon onClick={() => editHscCodeHandler( data.hsn_code_id)}/>
                          {/* <IconButton
                            onClick={(e) =>
                              deleteHscCode(e, data.hsn_code_id)
                            }
                          >
                            <DeleteIcon />
                          </IconButton> */}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
              <div className={Styles.pagination}>
                <Pagination
                  currentPage={currentPage}
                  totalPages={getFilterData?.total_page}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        </CustomLoader>
        <CustomDialog
          open={open}
          handleClose={handleClose}
          title="Delete HSN Code"
          content="Are you want to delete this Hsn Code?"
          handleConfirm={deleteHscCode}
        />
        <CustomDialogBox
          open={open}
          handleClose={handleClose}
          title="Edit HSN"
          content={
            <HsnForm
              setOpen={setOpen}
              open={open}
              setReload={setReload}
              mode={mode}
              hsnCodeId={hsnCodeId}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
        <MySnackbar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          severity={'success'}
          autoHideDuration={1000}
        />
      </div>
    </div>
  );
};

export default HsnCodeList;
