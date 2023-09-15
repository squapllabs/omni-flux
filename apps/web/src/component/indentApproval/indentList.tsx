import React, { useState, useEffect } from 'react';
import Styles from '../../styles/indentList.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import SearchIcon from '../menu/icons/search';
import CustomGroupButton from '../ui/CustomGroupButton';
import { getByUserRoleIndent } from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Pagination from '../menu/pagination';
import ViewIcon from '../menu/icons/viewIcon';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import { useNavigate } from 'react-router';
import { getBymasertDataTypeDrop } from '../../hooks/masertData-hook';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';

const IndentList = () => {
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  console.log("store data=====>",encryptedData.userData.user_roles[0].role_data.role_id);
  
  const userID: number = encryptedData.userId;
  const roleID:number = encryptedData.userData.user_roles[0].role_data.role_id;
  const [selectedValue, setSelectedValue] = useState('');
  const [selectedValueType, setSelectedValueType] = useState('');
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
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const {
    mutate: postDataForFilter,
    data: getIndentData,
    isLoading: FilterLoading,
  } = getByUserRoleIndent();
  console.log('dta====>', getIndentData);
  const { data: getPriorityType = [], isLoading: dropLoading } = getBymasertDataTypeDrop('PRTYPE');


  const handleReset = async () => {
    setFilterValues({
      search_by_name: '',
    });
    setIsResetDisabled(true);
    setSelectedValueType('')
  };
  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      approver_status: 'Pending',
      project_approver_id: userID,
      priority:''
    };
    postDataForFilter(userData);
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

  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedData = event.target.value;
    setSelectedValue(selectedData);
    setIsResetDisabled(searchValue === '');
  };

  const handleDropdownChangePriorityType = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedData = event.target.value;
    setSelectedValueType(selectedData);
    setIsResetDisabled(searchValue === '');
  };



  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Indent List</h3>
            <span className={Styles.content}>
              Indent list based on projects.
            </span>
          </div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              {/* <Input
                width="260px"
                prefixIcon={<SearchIcon />}
                name="search_by_name"
                value={filterValues.search_by_name}
                onChange={(e) => handleFilterChange(e)}
                placeholder="Search by name"
              /> */}
              <AutoCompleteSelect
                  name="parent_master_data_id"
                  defaultLabel="Select Project Name"
                  onChange={() => handleDropdownChange}
                  value={selectedValue}
                  placeholder="Project Name"
                  width="220px"
                  onSelect={(value) => {
                    setSelectedValue(value);
                    setIsResetDisabled(false);
                  }}
                //   optionList={
                //     dropLoading === true ? [] : getAllmasterDataForDrop
                //   }
                />
                 <AutoCompleteSelect
                  name="parent_master_data_id"
                  defaultLabel="Select Project Name"
                  onChange={() => handleDropdownChangePriorityType}
                  value={selectedValueType}
                  placeholder="Priority Type"
                  width="220px"
                  onSelect={(value) => {
                    setSelectedValueType(value);
                    setIsResetDisabled(false);
                  }}
                    optionList={  dropLoading === true ? [] : getPriorityType}
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
                disabled={isResetDisabled}
                onClick={handleReset}
              >
                Reset
              </Button>
            </div>
            {/* <div>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div> */}
          </div>
          <div className={Styles.dividerStyle}></div>
        </div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Project Name</th>
                  <th>Priority</th>
                  <th>Expected Delivery Date</th>
                  <th>Total Cost</th>
                  <th></th>
                </tr>
              </thead>
              <tbody>
                {getIndentData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    {activeButton === 'Pending' && <td></td>}
                  </tr>
                ) : (
                  ''
                )}
                {getIndentData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>{data?.priority}</td>
                      <td>
                        {format(
                          new Date(data?.expected_delivery_date),
                          'MMM dd, yyyy'
                        )}
                      </td>
                      <td>{formatBudgetValue(data?.total_cost)}</td>
                      {activeButton === 'Pending' && (
                        <td>
                          <div className={Styles.tablerow}>
                            <ViewIcon
                              onClick={() =>
                                navigate(
                                  `/indent-detail/${data?.indent_request_id}`
                                )
                              }
                            />
                          </div>
                        </td>
                      )}
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </div>
        <div className={Styles.pagination}>
          <Pagination
            currentPage={currentPage}
            totalPages={getIndentData?.total_page}
            totalCount={getIndentData?.total_count}
            rowsPerPage={rowsPerPage}
            onPageChange={handlePageChange}
            onRowsPerPageChange={handleRowsPerPageChange}
          />
        </div>
      </CustomLoader>
    </div>
  );
};

export default IndentList;
