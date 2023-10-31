import React, { useEffect, useState } from 'react';
import Styles from '../../../../styles/newStyles/projectIndentList.module.scss';
import {
  getProjectBasedIndent,
  getBySearchIndent,
  getIndentSearchPaginated,
} from '../../../../hooks/indentRequest-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import EditIcon from '../../../menu/icons/newEditIcon';
import { format } from 'date-fns';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import { formatBudgetValue } from '../../../../helper/common-function';
import Select from '../../../ui/selectNew';
import CustomLoader from '../../../ui/customLoader';
import CustomPagination from '../../../menu/CustomPagination';
import BOQIcon from '../../../menu/icons/boqIcon';
import Input from '../../../ui/Input';
import SearchIcon from '../../../menu/icons/search';
import ExpandIcon from '../../../menu/icons/expandIcon';
import ExpandClose from '../../../menu/icons/expandClose';
import Checkbox from '../../../ui/Checkbox';
import CustomGroupButton from '../../../ui/CustomGroupButton';
import { environment } from '../../../../environment/environment';
const ProjectIndentRequestList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const nullValues = environment.NULLVALUE;
  const approverStatus: any = [
    { value: '', label: 'All' },
    { value: 'Draft', label: 'Draft' },
    { value: 'Pending', label: 'Pending' },
    { value: 'Approved', label: 'Approved' },
    { value: 'Rejected', label: 'Rejected' },
  ];
  const [buttonLabels, setButtonLabels] = useState(approverStatus);
  const [activeButton, setActiveButton] = useState<string | null>('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(5);
  const [filterValues, setFilterValues] = useState({
    approver_status: '',
    search_by_code: '',
  });
  const [colps, setColps] = useState(false);
  const [intendId, setIndentID] = useState<any>({});
  const [po, setPo] = useState(false);
  const demo: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    project_id: Number(routeParams?.id),
    indent_request_code: filterValues.search_by_code,
    approver_status: filterValues.approver_status,
  };
  const {
    isLoading: FilterLoading,
    data: getFilterData,
    refetch,
  } = getIndentSearchPaginated(demo);
  console.log('demo', demo);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, filterValues]);
  useEffect(() => {
    if (filterValues?.search_by_code != '') {
      const handleSearch = setTimeout(() => {
        refetch();
      }, 1000);
      return () => clearTimeout(handleSearch);
    }
  }, [filterValues]);

  /* Function for changing the table page */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
    setFilterValues({ ...filterValues, ['approver_status']: value });
  };
  /* Function for changing no of rows in pagination */
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    // const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['approver_status']: event.target.value,
    });
  };

  const handleExpand = (value: any) => {
    console.log('filterValue-->', value);

    setColps(!colps);
    if (colps === true) {
      setIndentID(value);
      if (value?.purchase_request?.length === 0) {
        setPo(false);
      } else {
        value?.purchase_request?.forEach((data: any, index: any) => {
          console.log('purchase_request-->', data);
          if (data?.purchase_order.length > 0) {
            console.log('purchase_Q', data);
            setPo(true);
          }
        });
      }
    } else {
      setIndentID({});
    }
  };
  const { data: getIndentList } = getProjectBasedIndent(
    Number(routeParams?.id)
  );
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd-MM-yyyy');
    return formattedDate;
  };

  return (
    <div>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        {getFilterData?.is_available === true ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.headingOne}>
                  <div className={Styles.subHeading}>
                    <BOQIcon />
                    <h3>Indent Request</h3>
                  </div>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon color="white" />}
                      onClick={(e) => {
                        navigate(`/indent/${routeParams?.id}`);
                      }}
                    >
                      Add
                    </Button>
                  </div>
                </div>
                <div className={Styles.searchBar} style={{ gap: '10px' }}>
                  <Input
                    width="260px"
                    prefixIcon={<SearchIcon />}
                    name="search_by_code"
                    value={filterValues.search_by_code}
                    onChange={(e) => {
                      setFilterValues({
                        ...filterValues,
                        ['search_by_code']: e.target.value,
                      });
                      setCurrentPage(1);
                    }}
                    placeholder="Search by Code"
                  />
                  {/* <Select
                    width="200px"
                    name="approver_status"
                    value={filterValues.approver_status}
                    onChange={(e) => handleFilterChange(e)}
                    defaultLabel="Select from options"
                    placeholder="Select All"
                  >
                    {approverStatus?.map((items: any, index: any) => {
                      return (
                        <option key={items.value} value={items.value}>
                          {items.label}
                        </option>
                      );
                    })}
                  </Select> */}
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
            {/* <div className={Styles.searchField}>
              <div className={Styles.inputFilter}>
                <div className={Styles.filterSelect}>
                  <Select
                    label="Indent Status"
                    name="approver_status"
                    value={filterValues.approver_status}
                    onChange={(e) => handleFilterChange(e)}
                    defaultLabel="Select from options"
                    placeholder="Select from options"
                  >
                    {approverStatus?.map((items: any, index: any) => {
                      return (
                        <option key={items.value} value={items.value}>
                          {items.label}
                        </option>
                      );
                    })}
                  </Select>
                </div> */}
            {/* <div className={Styles.filterButton}>
                  <Button
                    // className={Styles.searchButton}
                    type="button"
                    color="primary"
                    shape="rectangle"
                    size="small"
                    justify="center"
                    onClick={(e) => handleSearch(e)}
                  >
                    Search
                  </Button>
                  <Button
                    className={Styles.resetButton}
                    type="button"
                    color="secondary"
                    shape="rectangle"
                    size="small"
                    justify="center"
                    onClick={(e) => handleReset(e)}
                  >
                    Reset
                  </Button>
                </div> */}
            {/* </div>
            </div> */}
            <div className={Styles.tableContainer}>
              <div>
                <table className={Styles.scrollable_table}>
                  <thead>
                    <tr>
                      <th className={Styles.tableHeading}></th>
                      <th className={Styles.tableHeading}>#</th>
                      <th className={Styles.tableHeading}>Indent Code</th>
                      <th className={Styles.tableHeading}>Requested by</th>
                      {/* <th className={Styles.tableHeadingSite}>
                        Indent Requested Date
                      </th>
                      <th className={Styles.tableHeading}>
                        Expected Delivery Date
                      </th> */}
                      <th className={Styles.tableHeading}>Indent Status</th>
                      <th className={Styles.tableHeading}>Approved Date</th>
                      <th className={Styles.tableHeading}>Approved By</th>
                      <th className={Styles.tableHeading}>Action</th>
                    </tr>
                  </thead>
                  <tbody>
                    {getFilterData?.content?.length > 0 ? (
                      getFilterData.content.map((items: any, index: any) => {
                        console.log('getFilterData', items);

                        rowIndex = rowIndex + 1;
                        return (
                          <>
                            <tr
                              key={index}
                              className={
                                items?.indent_request_id ===
                                intendId?.indent_request_id
                                  ? Styles.selectedRow
                                  : ''
                              }
                            >
                              <td>
                                <div
                                  onClick={() => handleExpand(items)}
                                  style={{
                                    display:
                                      items.approver_status === 'Approved'
                                        ? ''
                                        : 'none',
                                  }}
                                >
                                  {colps === false &&
                                  items?.indent_request_id ===
                                    intendId?.indent_request_id ? (
                                    <ExpandClose />
                                  ) : (
                                    <ExpandIcon />
                                  )}
                                </div>
                              </td>
                              <td>{rowIndex}</td>
                              <td>{items?.indent_request_code}</td>
                              <td>
                                {items?.requester_user_data?.first_name +
                                  ' ' +
                                  items?.requester_user_data?.last_name}
                              </td>
                              {/* <td>{dateFormat(items?.requested_date)}</td>
                              <td>
                                {dateFormat(items?.expected_delivery_date)}
                              </td> */}
                              {/* <td>{formatBudgetValue(items?.total_cost)}</td> */}
                              <td>{items?.approver_status}</td>
                              <td>
                                {items?.approved_date === null
                                  ? '--'
                                  : dateFormat(items?.approved_date)}
                              </td>
                              <td>
                                {items?.approver_user_data === null
                                  ? '--'
                                  : items?.approver_user_data?.first_name +
                                    ' ' +
                                    items?.approver_user_data?.last_name}
                              </td>
                              <td>
                                <div
                                  style={{
                                    cursor: 'pointer',
                                  }}
                                >
                                  <EditIcon
                                    onClick={(e) => {
                                      navigate(
                                        `/indent/${routeParams?.id}/${items?.indent_request_id}`
                                      );
                                    }}
                                  />
                                </div>
                              </td>
                            </tr>
                            {items?.indent_request_id ===
                              intendId?.indent_request_id && (
                              <tr>
                                <td></td>
                                <td colSpan={8}>
                                  <div className={Styles.Colps}>
                                    <div className={Styles.ColpsChilds}>
                                      <div
                                        className={Styles.ColpsheadingpanelOne}
                                      >
                                        <span className={Styles.heading}>
                                          INDENT REQUESTED DATE
                                        </span>
                                        <span className={Styles.heading}>
                                          EXPECTED DELIVERY DATE
                                        </span>
                                      </div>
                                      <div className={Styles.ColpsDatapanelOne}>
                                        <span>
                                          {dateFormat(intendId?.requested_date)}
                                        </span>
                                        <span>
                                          {dateFormat(
                                            intendId?.expected_delivery_date
                                          )}
                                        </span>
                                      </div>
                                    </div>
                                    <div className={Styles.ColpsChilds}>
                                      <div
                                        className={Styles.ColpsheadingpanelOne}
                                      >
                                        <span className={Styles.heading}>
                                          Purchase Request RAISED
                                        </span>
                                        <span className={Styles.heading}>
                                          Purchase Order RAISED
                                        </span>
                                      </div>
                                      <div className={Styles.ColpsDatapanelOne}>
                                        <span>
                                          <Checkbox
                                            checked={
                                              intendId?.purchase_request
                                                ?.length === 0
                                                ? false
                                                : true
                                            }
                                          />
                                        </span>
                                        <span>
                                          <Checkbox checked={po} />
                                        </span>
                                      </div>
                                    </div>
                                    <div className={Styles.ColpsChilds}>
                                      <div
                                        className={Styles.ColpsheadingpanelOne}
                                      >
                                        <span>No Purchase Request</span>
                                        <span></span>
                                      </div>
                                      <div className={Styles.ColpsDatapanelOne}>
                                        <span>
                                          {intendId?.purchase_request
                                            ?.length === 0
                                            ? nullValues
                                            : intendId?.purchase_request
                                                ?.length}
                                        </span>
                                        <span></span>
                                      </div>
                                    </div>
                                  </div>
                                </td>
                              </tr>
                            )}
                          </>
                        );
                      })
                    ) : (
                      <tr>
                        <td colSpan="6" style={{ textAlign: 'center' }}>
                          No data found
                        </td>
                      </tr>
                    )}
                  </tbody>
                </table>
              </div>
              <div className={Styles.pagination}>
                <CustomPagination
                  currentPage={currentPage}
                  totalPages={getFilterData?.total_page}
                  totalCount={getFilterData?.total_count}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        ) : (
          <div>
            <div className={Styles.subHeadingForInitialPage}>
              <BOQIcon width={30} height={30} color="black" />
              <h3>Indent Raise</h3>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.image}>
                <img src="/boq-add.png" width="100%" height="150px" />
              </div>
              <div>
                <h5 className={Styles.textmax}>
                  No indent added to this Project
                </h5>
              </div>
              <div>
                <p className={Styles.textmin}>
                  Go ahead, add a indent to this project now
                </p>
              </div>
              <div className={Styles.emptyButton}>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={(e) => {
                    navigate(`/indent/${routeParams?.id}`);
                  }}
                >
                  Add Indent
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
    </div>
  );
};
export default ProjectIndentRequestList;
