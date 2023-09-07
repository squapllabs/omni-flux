import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import Button from '../ui/Button';
import CustomLoader from '../ui/customLoader';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import Pagination from '../menu/pagination';
import EditIcon from '../menu/icons/editIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import AddIcon from '../menu/icons/addIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import Styles from '../../styles/vendor.module.scss';

const VendorList = () => {
  const navigate = useNavigate();

  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const [isLoading, setIsLoading] = useState(true);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [filter, setFilter] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');

  return (
    <div>
      <div>
        <div className={Styles.text}>
          <div className={Styles.textStyle}>
            <h3>List of Vendors</h3>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.searchField}>
          <div className={Styles.inputFilter}>
            <Input
              width="260px"
              prefixIcon={<SearchIcon />}
              name="search_by_name"
              value={filterValues.search_by_name}
              // onChange={(e) => handleFilterChange(e)}
              placeholder="Search"
            />
            <Button
              className={Styles.searchButton}
              shape="rectangle"
              justify="center"
              size="small"
              // onClick={handleSearch}
            >
              Search
            </Button>
            <Button
              className={Styles.resetButton}
              shape="rectangle"
              justify="center"
              size="small"
              // onClick={handleReset}
              disabled={isResetDisabled}
            >
              Reset
            </Button>
          </div>
          <div className={Styles.button}>
            <div>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon />}
                onClick={() => navigate('/add-vendor')}
              >
                Add
              </Button>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S. No</th>
                  <th>Vendor Name</th>
                  <th>Contact Person Name</th>
                  <th>Phone Number</th>
                  <th>Address</th>
                  {activeButton === 'AC' && <th>Options</th>}
                </tr>
              </thead>
              <tbody>

              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  );
};

export default VendorList;
