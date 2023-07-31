import React, { useState, useEffect } from 'react';
import Styles from '../../styles/gstList.module.scss';
import { useGetAllHsnCode, useDeleteHsnCode, uploadHsnCode, getByCode } from '../../hooks/hsnCode-hooks';
import { IconButton } from '@mui/material';
import CustomDialog from '../ui/customDialog';
import MySnackbar from '../ui/MySnackbar';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import HsnForm from './hsnCodeCreate';
import Button from '../ui/Button';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import { createHsnCode } from '../../hooks/hsnCode-hooks';
import * as XLSX from "xlsx"
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomGroupButton from '../ui/CustomGroupButton';

const HsnCodeList = () => {
  const { data: getAllHsnData, isLoading: getAllLoading } = useGetAllHsnCode();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getByCode();
  const { mutate: getDeleteHsnCodeByID } = useDeleteHsnCode();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [open, setOpen] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [hsnCodeId, setHsnCodeId] = useState();
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(10); // Set initial value to 1
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'active', value: 'AC' },
    { label: 'inactive', value: 'IC' },
  ]);

  const deleteHscCode = (event: React.FormEvent, value: any) => {
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

  const editHscCodeHandler = (event: React.FormEvent, value: any) => {
    setMode('EDIT');
    setHsnCodeId(value);
    setOpen(true);
  };

  const [initialValues, setInitialValues] = useState({
    hsn_code_id: '',
    code: '',
    description: '',
  });

  const [filterValues, setFilterValues] = useState({
    search_by_code: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_code']: event.target.value,
    });
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]
  );

  const handleSearch = async () => {
    let demo: any = {
      limit: 5,
      offset: 0,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: '',
      status: 'AC',
      ...filterValues,
    };
postDataForFilter(demo);
setIsLoading(false);
setFilter(true);
  };

const handleReset = async () => {
  let demo: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
  };
  postDataForFilter(demo);
  setIsLoading(false);
  setFilter(false);
  setFilterValues({
    search_by_code: '',
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

const { mutate: createNewHsnCode } = createHsnCode();
const formik = useFormik({
  initialValues,
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
            setMessage('UOM created');
            setOpenSnack(true);
            resetForm();
          }
        },
      });
    }
  },
});

const [jsonData, setJsonData] = useState<any[]>([]);

const handleFileChange = (e: any) => {
  const file = e.target.files[0];
  if (file) {
    const reader = new FileReader();
    reader.onload = function (e) {
      const data = e.target?.result as ArrayBuffer;
      if (data) {
        const workbook = XLSX.read(data, { type: 'array' });
        const jsonData = XLSX.utils.sheet_to_json(workbook.Sheets[workbook.SheetNames[0]]);
        setJsonData(jsonData);
      }
    };
    reader.readAsArrayBuffer(file);
  }
};

const { mutate: uploadJsonData } = uploadHsnCode();
const handleUpload = () => {
  if (jsonData.length > 0) {
    uploadJsonData(jsonData, {
      onSuccess: (data, variables, context) => {
        if (data) {
          setMessage('Data uploaded successfully!');
          setOpenSnack(true);
        }
      },
    });
  } else {
    console.log('No data to upload.');
  }
};

return (
  <div>

    <div>
      <CustomLoader
        loading={isLoading === true ? getAllLoading : FilterLoading}
        // loading={true}
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
                  placeholder="Enter product code"
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
                  placeholder="Enter product description"
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
          <div>
            <div className={Styles.fields}>
              <Input
                type="file"
                onChange={handleFileChange}
              />
            </div>
            <div className={Styles.fields}>
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
          </div>
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
                name="search_by_code"
                value={filterValues.search_by_code}
                onChange={(e) => handleFilterChange(e)}
                placeholder="Search by item code"
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
                  {getAllHsnData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td>No data found</td>
                      <td></td>
                    </tr>
                  ) : (
                    ''
                  )}
                  {getAllHsnData?.map((data: any) => (
                    <tr>
                      <td>{data.code}</td>
                      <td>{data.description}</td>
                      <td>
                        <IconButton
                          onClick={(e) => editHscCodeHandler(e, data.hsn_code_id)}
                        >
                          <EditIcon />
                        </IconButton>
                        <IconButton
                          onClick={(e) =>
                            deleteHscCode(e, data.hsn_code_id)
                          }
                        >
                          <DeleteIcon />
                        </IconButton>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            <div className={Styles.pagination}>
              <Pagination
                currentPage={currentPage}
                totalPages={totalPages}
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
        title="HSN Creation"
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
