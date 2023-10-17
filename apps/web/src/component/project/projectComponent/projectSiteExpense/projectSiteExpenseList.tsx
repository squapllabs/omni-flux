import React, { useState, useEffect } from 'react';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import CustomSidePopup from '../../../ui/CustomSidePopup';
import ProjectSiteExpenseForm from './projectSiteExpenseForm';
import { useNavigate, useParams } from 'react-router-dom';
import Styles from '../../../../styles/newStyles/siteExpenseList.module.scss';
import MoneyIcon from '../../../menu/icons/moneyIcon';
import { getProjectSite } from '../../../../hooks/project-hooks';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import CustomGroupButton from '../../../ui/CustomGroupButton';
import { getBySearchsiteExpense } from '../../../../hooks/expense-hook';
import { format } from 'date-fns';
import EditIcon from '../../../menu/icons/newEditIcon';
import CustomLoader from '../../../ui/customLoader';
import CustomPagination from '../../../menu/CustomPagination';
import { formatBudgetValue } from '../../../../helper/common-function';
import ViewIcon from '../../../menu/icons/newViewIcon';
import ExpenseDetailApprove from './approval/siteExpenseDetailApprove';

const ProjectSiteExpenseList = () => {
  const routeParams = useParams();
  let rowIndex = 0;
  const navigate = useNavigate();
  const [open, setOpen] = useState(false);
  const [activeButton, setActiveButton] = useState<string | null>('All');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [expenseID, setExpenseID] = useState();
  const [mode, setMode] = useState('');
  const [reload, setReload] = useState(false);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'All', value: 'All' },
    { label: 'Draft', value: 'Draft' },
    { label: 'Waiting Approval', value: 'Pending' },
    { label: 'InProgress', value: 'InProgress' },
    { label: 'Completed', value: 'Completed' },
  ]);

  const { data: getSiteList, isLoading: siteLoading } = getProjectSite(
    Number(routeParams?.id)
  );
  const initialSiteId =
    !siteLoading && getSiteList ? getSiteList[0]?.value : null;
  const [filterValue, setFilterValue] = useState(initialSiteId);

  const {
    mutate: postDataForFilter,
    data: getExpenseList,
    isLoading: fetchLoader,
  } = getBySearchsiteExpense();

  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      project_id: Number(routeParams?.id),
      expense_status: activeButton,
      site_id: filterValue === null ? initialSiteId : filterValue,
    };
    postDataForFilter(demo);
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
  const handleClose = () => {
    setOpen(false);
  };
  const handleEditExpense = (e: any, expenseId: any) => {
    setMode('Edit');
    setOpen(true);
    setExpenseID(expenseId);
  };

  const handleViewExpense = (expenseId: any) => {
    setExpenseID(expenseId);
    setOpen(true);
  };

  useEffect(() => {
    handleSearch();
  }, [
    currentPage,
    rowsPerPage,
    filterValue,
    activeButton,
    reload,
    getSiteList,
  ]);
  return (
    <div className={Styles.container}>
      <CustomLoader loading={fetchLoader} size={48}>
        {getSiteList ? (
          <div>
            <div className={Styles.topHeading}>
              <MoneyIcon color="black" />
              <span>Site Claim for</span>
              <div>
                {getSiteList && (
                  <AutoCompleteSelect
                    name="site_id"
                    label="Site"
                    mandatory={true}
                    optionList={getSiteList}
                    value={
                      filterValue === null ? Number(initialSiteId) : filterValue
                    }
                    onSelect={(value: any) => {
                      setFilterValue(value);
                    }}
                  />
                )}
              </div>
              <div className={Styles.sub_header}>
                {getExpenseList?.expense_statistics?.total_expenses === 0 ? (
                  ''
                ) : (
                  <div style={{ padding: '8px', display: 'flex' }}>
                    <div className={Styles.vertical}>
                      <div className={Styles.verticalLine}></div>
                    </div>
                  </div>
                )}
              </div>
              <div>
                {getExpenseList?.expense_statistics?.total_expenses === 0 ? (
                  ' '
                ) : (
                  <Button
                    type="button"
                    color="primary"
                    shape="rectangle"
                    size="small"
                    justify="center"
                    icon={<AddIcon width={20} color="white" />}
                    onClick={(e) => {
                      setOpen(true);
                      setMode('Add');
                    }}
                  >
                    Add Claim
                  </Button>
                )}
              </div>
            </div>
            <div>
              {getExpenseList?.total_count !== 0 ||
              getExpenseList?.expense_statistics?.total_expenses !== 0 ? (
                <div>
                  <div className={Styles.cards}>
                    <div className={Styles.amountCards}>
                      <div className={Styles.card1}>
                        <div className={Styles.textStyle}>
                          <span>Total Invoices</span>
                          <p>
                            {getExpenseList?.expense_statistics?.total_expenses
                              ? getExpenseList?.expense_statistics
                                  ?.total_expenses
                              : 0}
                          </p>
                        </div>
                      </div>
                      <div className={Styles.card2}>
                        <div className={Styles.textStyle}>
                          <span className={Styles.approvedStyles}>
                            Approved Claims
                          </span>
                          <p>
                            {formatBudgetValue(
                              getExpenseList?.expense_statistics
                                ?.approved_expenses
                                ? getExpenseList?.expense_statistics
                                    ?.approved_expenses
                                : 0
                            )}
                          </p>
                        </div>
                      </div>
                      <div className={Styles.card2}>
                        <div className={Styles.textStyle}>
                          <span className={Styles.rejectedStyles}>
                            Rejected Claims
                          </span>
                          <p>
                            {formatBudgetValue(
                              getExpenseList?.expense_statistics
                                ?.rejected_expenses
                                ? getExpenseList?.expense_statistics
                                    ?.rejected_expenses
                                : 0
                            )}
                          </p>
                        </div>
                      </div>
                      <div className={Styles.card2}>
                        <div className={Styles.textStyle}>
                          <span className={Styles.pendingStyles}>
                            Pending Claims
                          </span>
                          <p>
                            {formatBudgetValue(
                              getExpenseList?.expense_statistics
                                ?.pending_expenses
                                ? getExpenseList?.expense_statistics
                                    ?.pending_expenses
                                : 0
                            )}
                          </p>
                        </div>
                      </div>
                    </div>
                  </div>
                  <div className={Styles.grpButtons}>
                    <CustomGroupButton
                      labels={buttonLabels}
                      onClick={handleGroupButtonClick}
                      activeButton={activeButton}
                    />
                  </div>
                  <div>
                    <table className={Styles.scrollable_table}>
                      <thead>
                        <tr>
                          <th className={Styles.tableHeading}>#</th>
                          <th className={Styles.tableHeading}>Expense Code</th>
                          <th className={Styles.tableHeading}>Added By</th>
                          <th className={Styles.tableHeading}>Site</th>
                          <th className={Styles.tableHeading}>Status</th>
                          <th className={Styles.tableHeading}>Amount</th>
                          <th className={Styles.tableHeading}>Action</th>
                        </tr>
                      </thead>
                      <tbody>
                        {getExpenseList?.content?.length === 0 ? (
                          <tr>
                            <td colSpan="7" style={{ textAlign: 'center' }}>
                              No data found
                            </td>
                          </tr>
                        ) : (
                          ''
                        )}
                        {getExpenseList?.content?.map(
                          (items: any, index: any) => {
                            if (items.is_delete != true) {
                              rowIndex = rowIndex + 1;
                              return (
                                <tr>
                                  <td>{rowIndex}</td>
                                  <td>{items?.expense_code}</td>
                                  <td>{items?.employee_name}</td>
                                  <td>{items?.site_data?.name}</td>
                                  <td>
                                    <span
                                      className={`${Styles.status} ${
                                        items?.status === 'Pending'
                                          ? Styles.pendingStatus
                                          : items?.status === 'InProgress'
                                          ? Styles.rejectedStatus
                                          : items?.status === 'Approved'
                                          ? Styles.approvedStatus
                                          : items?.status === 'Draft'
                                          ? Styles.draftStatus
                                          : items?.status === 'Completed'
                                          ? Styles.approvedStatus
                                          : ''
                                      }`}
                                    >
                                      {items?.status}
                                    </span>
                                  </td>
                                  <td>
                                    {formatBudgetValue(
                                      items?.total_amount
                                        ? items?.total_amount
                                        : 0
                                    )}
                                  </td>
                                  <td>
                                    {items?.status === 'Draft' ? (
                                      <div
                                        style={{ cursor: 'pointer' }}
                                        onClick={(e) => {
                                          handleEditExpense(
                                            e,
                                            items.expense_id
                                          );
                                        }}
                                      >
                                        <EditIcon />
                                      </div>
                                    ) : (
                                      <div
                                        style={{ cursor: 'pointer' }}
                                        onClick={(e) => {
                                          handleViewExpense(items.expense_id);
                                        }}
                                      >
                                        <ViewIcon />
                                      </div>
                                    )}
                                  </td>
                                </tr>
                              );
                            }
                          }
                        )}
                      </tbody>
                    </table>
                  </div>
                  <div>
                    <CustomPagination
                      currentPage={currentPage}
                      totalPages={getExpenseList?.total_page}
                      totalCount={getExpenseList?.total_count}
                      rowsPerPage={rowsPerPage}
                      onPageChange={handlePageChange}
                      onRowsPerPageChange={handleRowsPerPageChange}
                    />
                  </div>
                </div>
              ) : (
                <div className={Styles.emptyData}>
                  <MoneyIcon height={60} width={60} color="#475467" />
                  <h5>No Site Expenses added for this site </h5>
                  <span className={Styles.spanContent}>
                    Let's add an expanse now
                  </span>
                  <Button
                    type="button"
                    color="primary"
                    shape="rectangle"
                    size="small"
                    justify="center"
                    icon={<AddIcon width={20} color="white" />}
                    onClick={(e) => {
                      setOpen(true);
                      setMode('Add');
                    }}
                  >
                    Add Claim
                  </Button>
                </div>
              )}
            </div>
            <CustomSidePopup
              open={open}
              handleClose={handleClose}
              title={mode === 'Edit' ? 'Edit Site Claims ' : 'Add Site Claims'}
              content={
                <ProjectSiteExpenseForm
                  projectId={routeParams?.id}
                  setOpen={setOpen}
                  open={open}
                  setExpenseID={setExpenseID}
                  expenseID={expenseID}
                  setMode={setMode}
                  mode={mode}
                  siteId={
                    filterValue === null ? Number(initialSiteId) : filterValue
                  }
                  setReload={setReload}
                  reload={reload}
                />
              }
              width={'90%'}
            />
            <CustomSidePopup
              open={open}
              handleClose={handleClose}
              title={'Claim Details'}
              content={
                <ExpenseDetailApprove
                  expenseID={expenseID}
                  setOpen={setOpen}
                  open={open}
                />
              }
              width={'90%'}
            />
          </div>
        ) : (
          <div>
            <div className={Styles.subHeading}>
              <MoneyIcon width={30} height={30} color="black" />
              <h3>SITE Claim</h3>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.image}>
                <img src="/siteAdd.png" width="70%" height="20%" />
              </div>
              <div>
                <h5 className={Styles.textmax}>
                  No sites added to this Project
                </h5>
              </div>
              <div>
                <p className={Styles.textmin}>
                  Create a site for this project by going to the previous menu.
                </p>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
    </div>
  );
};

export default ProjectSiteExpenseList;
