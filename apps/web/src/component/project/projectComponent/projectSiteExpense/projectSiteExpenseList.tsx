import React, { useState,useEffect } from 'react';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import CustomSidePopup from '../../../ui/CustomSidePopup';
import ProjectSiteExpenseForm from './projectSiteExpenseForm';
import { useNavigate, useParams } from 'react-router-dom';
import Styles from '../../../../styles/newStyles/siteExpenseList.module.scss';
import MoneyIcon from '../../../menu/icons/MoneyIcon';
import { getProjectSite } from '../../../../hooks/project-hooks';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import CustomGroupButton from '../../../ui/CustomGroupButton';
import { getBySearchsiteExpense } from '../../../../hooks/expense-hook';

const ProjectSiteExpenseList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const [open, setOpen] = useState(false);
  const { data: getSiteList } = getProjectSite(Number(routeParams?.id));
  const [activeButton, setActiveButton] = useState<string | null>('All');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [filterValue, setFilterValue] = useState<any>({});
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'All', value: 'All' },
    { label: 'Approved', value: 'Approved' },
    { label: 'Pending', value: 'Pending' },
    { label: 'Rejected', value: 'Rejected' },
    { label: 'Draft', value: 'Draft' },
  ]);
  const {
    mutate: postDataForFilter,
    data: getExpenseList,
    isLoading: fetchLoader,
  } = getBySearchsiteExpense();
console.log("getExpenseList",getExpenseList);

  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      project_id: Number(routeParams?.id),
      expense_status:activeButton,
      ...filterValue,
    };
    postDataForFilter(demo);
  };

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const handleClose = () => {
    setOpen(false);
  };
  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);
  return (
    <div className={Styles.container}>
      <div className={Styles.topHeading}>
        <MoneyIcon />
        <span>Site Expenses for</span>
        <div>
          <AutoCompleteSelect
            name="site_id"
            label="Site"
            mandatory={true}
            optionList={getSiteList}
          />
        </div>
        <div>
          <Button
            type="button"
            color="primary"
            shape="rectangle"
            size="small"
            justify="center"
            icon={<AddIcon width={20} color="white" />}
            onClick={(e) => {
              setOpen(true);
            }}
          >
            Add Expense
          </Button>
        </div>
      </div>
      <div className={Styles.cards}>
        <div className={Styles.amountCards}>
          <div className={Styles.card1}>
            <div className={Styles.textStyle}>
              <h3>
                <b>Total Invoice</b>
              </h3>
              <p>20000</p>
            </div>
          </div>
          <div className={Styles.card2}>
            <div className={Styles.textStyle}>
              <h3>
                <b>Approved Expenses</b>
              </h3>
              <p>20000</p>
            </div>
          </div>
          <div className={Styles.card2}>
            <div className={Styles.textStyle}>
              <h3>
                <b>Rejected Expenses</b>
              </h3>
              <p>20000</p>
            </div>
          </div>
          <div className={Styles.card2}>
            <div className={Styles.textStyle}>
              <h3>
                <b>Pending Expenses</b>
              </h3>
              <p>20000</p>
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
                <th className={Styles.tableHeading}>Site</th>
                <th className={Styles.tableHeading}>From Date</th>
                <th className={Styles.tableHeading}>To Date</th>
                <th className={Styles.tableHeading}>Amount</th>
                <th className={Styles.tableHeading}>Status</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            {/* <tbody>
              {getExpenseList?.content?.length === 0 ? (
                <tr>
                  <td colSpan="7" style={{textAlign:'center'}}>No data found</td>
                </tr>
              ) : (
                ''
              )}
              {getExpenseList?.content?.map((items: any, index: any) => {
                if (items.is_delete != true) {
                  rowIndex = rowIndex + 1;
                  console.log('items', items);
                  const sumOfRates = items?.expense_details.reduce(
                    (accumulator: any, currentItem: any) => {
                      return accumulator + currentItem.total;
                    },
                    0
                  );
                  return (
                    <tr>
                      <td>{rowIndex}</td>
                      <td>{items?.site_data?.name}</td>
                      <td>{dateFormat(items?.start_date)}</td>
                      <td>{dateFormat(items?.end_date)}</td>
                      <td>{sumOfRates}</td>
                      <td>{items?.status}</td>
                      <td>
                        <div
                          style={{ cursor: 'pointer' }}
                          onClick={() => {
                            navigate(
                              `/expenses-edit/${routeParams?.id}/${items.expense_id}`
                            );
                          }}
                        >
                          <EditIcon />
                        </div>
                      </td>
                    </tr>
                  );
                }
              })}
            </tbody> */}
          </table>
      </div>
      <CustomSidePopup
        open={open}
        handleClose={handleClose}
        title={'Add Site Expense'}
        content={<ProjectSiteExpenseForm />}
      />
    </div>
  );
};

export default ProjectSiteExpenseList;
