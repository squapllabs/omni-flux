import React, { useState, useEffect } from 'react';
import Styles from '../../styles/purchaseList.module.scss';
import Button from '../ui/Button';
import { getByUserRoleIndent } from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Pagination from '../menu/pagination';
import ViewIcon from '../menu/icons/newViewIcon';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useNavigate } from 'react-router-dom';
import PdfDownloadIcon from '../menu/icons/pdfDownloadIcon';
import ReportGenerator from '../reportGenerator/pdfReport/invoice';
import CustomPagination from '../menu/CustomPagination';
import ProjectSubheader from '../project/projectSubheader';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import Select from '../ui/selectNew';

const PurchaseList = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const navigate = useNavigate();
  const { data: getAllmasterDataForDrop = [] } = useGetAllProjectDrop();
  //   const [buttonLabels, setButtonLabels] = useState([
  //     { label: 'Pending', value: 'Pending' },
  //     { label: 'Approved', value: 'Approved' },
  //     { label: 'Rejected', value: 'Rejected' },
  //   ]);
  const [selectedValue, setSelectedValue] = useState('');
  const [priorityValue, setPriorityValue] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [projectId, setProjectId] = useState();
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [filterValues, setFilterValues] = useState({
    search_by_code: '',
    priority: '',
  });
  const {
    mutate: postDataForFilter,
    data: getIndentData,
    isLoading: FilterLoading,
  } = getByUserRoleIndent();
console.log("yyyy",getIndentData);

  const handleReset = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      project_id: '',
      priority: '',
      status: 'AC',
      approver_status: 'Approved',
      // request_type: 'Head Office',
    };
    postDataForFilter(userData);
    setSelectedValue('');
    setPriorityValue('');
    setFilterValues({
      search_by_code: '',
      priority: '',
    });
    setIsResetDisabled(true);
  };
  const handleReportGenerator = () => {
    const data: any = {
      title: 'Purchase Request',
      name: 'purchase_request',
    };
    ReportGenerator(data);
  };
  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      project_id: Number(selectedValue),
      priority: priorityValue,
      status: 'AC',
      approver_status: 'Approved',
      indent_request_code: filterValues.search_by_code,
      request_type: 'Head Office',
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

  const handleView = (indentId: any, projectId: any) => {
    navigate(`/purchase-detail/${indentId}`, {
      state: { project_id: projectId },
    });
  };

  const options: any = [
    { value: 'Low', label: 'Low' },
    { value: 'Medium', label: 'Medium' },
    { value: 'High', label: 'High' },
  ];

  const handleProjectDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedProjectId = event.target.value;
    setSelectedValue(selectedProjectId);
    setIsResetDisabled(searchValue === '');
  };
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedPriority = event.target.value;
    console.log('searchValue', searchValue);

    setFilterValues({
      ...filterValues,
      ['priority']: searchValue,
    });
    setPriorityValue(selectedPriority);
    setIsResetDisabled(searchValue === '');
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          {/* <div className={Styles.textContent}>
            <h3>Purchase List</h3>
            <span className={Styles.content}>Purchase List based on Project</span>
          </div> */}
          <ProjectSubheader
            navigation={'/home'}
            description="Approved Indent List based on Project"
            title="Approved Indent List"
          />
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <AutoCompleteSelect
                name="project_id"
                label="Select Project"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={selectedValue}
                onChange={() => handleProjectDropdownChange}
                onSelect={(value) => {
                  setSelectedValue(value);
                  setIsResetDisabled(false);
                }}
                optionList={getAllmasterDataForDrop}
              />
              <Select
                width="180px"
                name="priority"
                label="Priority"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={filterValues?.priority}
                onChange={(e) => handleDropdownChange(e)}
              >
                {options?.map((item: any, index: any) => {
                  return <option value={item.value}>{item.label}</option>;
                })}
              </Select>
              {/* <AutoCompleteSelect
                name="priority"
                label="Priority"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={filterValues?.priority}
                onChange={() => handleDropdownChange}
                onSelect={(value) => {
                  setFilterValues({
                    ...filterValues,
                    ['priority']: value,
                  });
                  setPriorityValue(value);
                  setIsResetDisabled(false);
                }}
                optionList={options}
              /> */}
              <Input
                width="260px"
                prefixIcon={<SearchIcon />}
                label="Indent Code"
                name="search_by_code"
                value={filterValues.search_by_code}
                onChange={(e) => {
                  setFilterValues({
                    ...filterValues,
                    ['search_by_code']: e.target.value,
                  });
                  setCurrentPage(1);
                  setIsResetDisabled(false);
                }}
                placeholder="Search by Code"
              />
              <div className={Styles.buttonStyle}>
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
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th className={Styles.tableHeading}>#</th>
                  <th className={Styles.tableHeading}>Indent Code</th>
                  <th className={Styles.tableHeading}>Project Name</th>
                  <th className={Styles.tableHeading}>Raised By</th>
                  <th className={Styles.tableHeading}>
                    Expected Delivery Date
                  </th>
                  <th className={Styles.tableHeading}>Priority</th>
                  <th className={Styles.tableHeading}>Cost</th>
                  <th className={Styles.tableHeading}>Approved By</th>
                  <th className={Styles.tableHeading}>Approved Date</th>
                  <th className={Styles.tableHeading}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {getIndentData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    <td></td>
                  </tr>
                ) : (
                  ''
                )}
                {getIndentData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.indent_request_code}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>
                        {data?.requester_user_data?.first_name +
                          ' ' +
                          data?.requester_user_data?.last_name}
                      </td> 
                      <td>
                        {format(
                          new Date(data?.expected_delivery_date),
                          'MMM dd, yyyy'
                        )}
                      </td>
                      {/* <td>{data?.priority}</td> */}
                      <td
                        className={
                          data?.priority === 'HIGH' ? Styles.highPriority : ''
                        }
                      >
                        {data?.priority}
                      </td>
                      <td>
                        {formatBudgetValue(
                          data?.total_cost ? data?.total_cost : 0
                        )}
                      </td>
                      <td>
                        {data?.approver_user_data?.first_name +
                          ' ' +
                          data?.approver_user_data?.last_name}
                      </td>
                      <td>
                        {' '}
                        {format(new Date(data?.approved_date), 'MMM dd, yyyy')}
                      </td>
                      <td>
                        <div
                          style={{
                            display: 'flex',
                            alignItems: 'center',
                            gap: '10px',
                          }}
                        >
                          <div className={Styles.tablerow}>
                            <ViewIcon
                              onClick={() =>
                                handleView(
                                  data?.indent_request_id,
                                  data?.project_id
                                )
                              }
                            />
                            {/* <PdfDownloadIcon onClick={() => handleReportGenerator()} /> */}
                          </div>
                          <div
                            className={Styles.tablerow}
                            style={{ color: 'green' }}
                          >
                            <span
                              onClick={() =>
                                navigate(
                                  `/purchase-request-list/${data.indent_request_id}`
                                )
                              }
                            >
                              PR
                            </span>
                            {/* <PdfDownloadIcon onClick={() => handleReportGenerator()} /> */}
                          </div>
                        </div>
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </div>
        <div className={Styles.pagination}>
          <CustomPagination
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

export default PurchaseList;
