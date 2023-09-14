import React, { useState, useEffect } from 'react';
import Styles from '../../styles/indentList.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import SearchIcon from '../menu/icons/search';
import CustomGroupButton from '../ui/CustomGroupButton';
import { getByUserRoleIndent } from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const IndentList = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;

  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Pending', value: 'Pending' },
    { label: 'Approved', value: 'Approved' },
    { label: 'Rejected', value: 'Rejected' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('Pending');
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const {
    mutate: postDataForFilter,
    data: getIndentData,
    isLoading: FilterLoading,
  } = getByUserRoleIndent();
  console.log('dta====>', getIndentData);

  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      status: activeButton,
      approvar_user_id: userID,
    };
    postDataForFilter(userData);
  };

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);
  return (
    <div className={Styles.container}>
      <div className={Styles.box}>
        <div className={Styles.textContent}>
          <h3>Indent Raise List</h3>
          <span className={Styles.content}>
            Manage your project data across your application
          </span>
        </div>
        <div className={Styles.searchField}>
          <div className={Styles.inputFilter}>
            <Input
              width="260px"
              prefixIcon={<SearchIcon />}
              name="search_by_name"
              //   value={filterValues.search_by_name}
              //   onChange={(e) => handleFilterChange(e)}
              placeholder="Search by name"
            />
            <Button
              className={Styles.searchButton}
              shape="rectangle"
              justify="center"
              size="small"
              //   onClick={handleSearch}
            >
              Search
            </Button>
            <Button
              className={Styles.resetButton}
              shape="rectangle"
              justify="center"
              size="small"
              //   disabled={isResetDisabled}
              //   onClick={handleReset}
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
      </div>
    </div>
  );
};

export default IndentList;
