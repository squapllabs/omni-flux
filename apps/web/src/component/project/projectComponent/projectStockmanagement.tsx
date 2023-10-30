import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/newStyles/projectStockManagement.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import { useNavigate, useParams } from 'react-router-dom';
import { useGetAllPaginatedStockAudit } from '../../../hooks/stockAudit-hooks';
import CustomLoader from '../../ui/customLoader';
import { format } from 'date-fns';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { getProjectSite } from '../../../hooks/project-hooks';
import ViewIcon from '../../menu/icons/newViewIcon';
import CustomPagination from '../../menu/CustomPagination';
import StockIcon from '../../menu/icons/stockIcon';
import CustomSidePopup from '../../ui/CustomSidePopup';
import CustomSnackBar from '../../ui/customSnackBar';
import ProjectStockAdd from './projectStockAdd';
import StockAuditService from '../../../service/stockaudit-service';

const ProjectStockmanagement = () => {
  const routeParams = useParams();
  const projectId = Number(routeParams?.id);
  const navigate = useNavigate();
  let rowIndex = 0;

  const { data: getSiteList, isLoading: siteLoading } = getProjectSite(
    Number(routeParams?.id)
  );

  const initialSiteId =
    !siteLoading && getSiteList ? getSiteList[0]?.value : null;
  const [intialValue, setIntialValue] = useState(initialSiteId);
  const [filterValue, setFilterValue] = useState(initialSiteId);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [reload, setReload] = useState(false);
  const [arrLength, setArrLength] = useState<boolean>();
  const [warning, setWarning] = useState(false);
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };

  const stockData: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    project_id: Number(routeParams?.id),
    site_id: filterValue === null ? initialSiteId : filterValue,
  };
  const {
    data: getStockAuditList,
    isLoading: fetchLoader,
    refetch,
  } = useGetAllPaginatedStockAudit(stockData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, filterValue, reload, getSiteList, arrLength]);
  useEffect(() => {
    refetch();
    // seekData(intialValue);
    const fetchData = async () => {
      const values = {
        projectId: projectId,
        siteId: intialValue === null ? getSiteList[0]?.value : intialValue,
      };
      try {
        const itemsData = await StockAuditService.getItems(values);
        const num = itemsData?.data?.length;
        if (itemsData?.data?.length > 0) {
          setArrLength(true);
        } else {
          setArrLength(false);
        }
      } catch (err) {
        console.log('error in list : ', err);
      }
    };
    fetchData();
  }, [!siteLoading, intialValue]);

  /* Function for changing the table page */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  /* Function for changing no of rows in pagination */
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const handleAddStockData = () => {
    if (intialValue !== '' && arrLength === true) {
      setOpen(true);
      setArrLength(false);
    } else {
      if (arrLength === false && intialValue !== '') {
        setOpenSnack(true);
        setWarning(true);
        setMessage('No items have been approved to this site!');
      } else {
        setOpenSnack(true);
        setWarning(true);
        setMessage('Select any one Site!');
      }
    }
  };

  const handleClosePopup = () => {
    setOpen(false);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
    setWarning(false);
  };

  // const seekData = async (value: any) => {
  //   const values = {
  //     projectId: projectId,
  //     siteId: intialValue === null ? getSiteList[0]?.value : intialValue,
  //   };
  //   try {
  //     const itemsData = await StockAuditService.getItems(values);
  //     const num = itemsData?.data?.length;
  //     if (itemsData?.data?.length > 0) {
  //       setArrLength(true);
  //     } else {
  //       setArrLength(false);
  //     }
  //   } catch (err) {
  //     console.log('error in list : ', err);
  //   }
  // };

  return (
    <div className={Styles.container}>
      <CustomLoader loading={fetchLoader} size={48} color="#333C44">
        {/* Header Part */}
        <div className={Styles.topHeading}>
          <div className={Styles.heading}>
            <div className={Styles.subHeading}>
              <StockIcon />
              <h4>STOCK AUDIT</h4>
            </div>
            <div className={Styles.filterSelect}>
              <AutoCompleteSelect
                name="site_id"
                label="Site"
                optionList={getSiteList != null ? getSiteList : []}
                value={
                  filterValue === null ? Number(initialSiteId) : filterValue
                }
                onSelect={(value) => {
                  if (value !== '') {
                    setFilterValue(value);
                  }
                  setIntialValue(value);
                }}
                showClearIcon={false}
              />
            </div>
          </div>
          {getStockAuditList?.total_count > 0 ? (
            <div>
              <Button
                type="button"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
                icon={<AddIcon width={20} color="white" />}
                onClick={handleAddStockData}
              >
                Add
              </Button>
            </div>
          ) : (
            ''
          )}
          {/* <div>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
            >
              Stock Adjustment
            </Button>
          </div> */}
        </div>
        {getStockAuditList?.total_count > 0 ? (
          <div className={Styles.tableContainer}>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th>S.No</th>
                  <th>Site</th>
                  <th>Audit Date</th>
                  <th>Item count</th>
                  <th>Action</th>
                </tr>
              </thead>
              <tbody>
                {getStockAuditList?.content?.length > 0 ? (
                  getStockAuditList.content.map((items: any, index: any) => {
                    rowIndex = rowIndex + 1;
                    return (
                      <tr key={index}>
                        <td>{rowIndex}</td>
                        <td>{items?.site_data?.name}</td>
                        <td>{dateFormat(items?.stock_audit_date)}</td>
                        <td>{items?.item_details?.length}</td>
                        <td>
                          <div
                            style={{ cursor: 'pointer' }}
                            onClick={() => {
                              navigate(
                                `/project-stockView/${items?.stock_audit_id}`
                              );
                            }}
                          >
                            <ViewIcon />
                          </div>
                        </td>
                      </tr>
                    );
                  })
                ) : (
                  <tr>
                    <td colSpan="5" style={{ textAlign: 'center' }}>
                      No data found
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
            <div className={Styles.pagination}>
              <CustomPagination
                currentPage={currentPage}
                totalPages={getStockAuditList?.total_page}
                totalCount={getStockAuditList?.total_count}
                rowsPerPage={rowsPerPage}
                onPageChange={handlePageChange}
                onRowsPerPageChange={handleRowsPerPageChange}
              />
            </div>
          </div>
        ) : (
          <div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.emptyDataHandling}>
                <img
                  src="/StockManagement.jpg"
                  alt="aa"
                  width="35%"
                  height="40%"
                />
              </div>
              <div>
                <h5>This project has no Stock Details</h5>
              </div>
              <div>
                <span className={Styles.spanContent}>
                  Manage Stock Audit details to this project now
                </span>
              </div>
              <div className={Styles.emptyButton}>
                <Button
                  type="button"
                  color="primary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  icon={<AddIcon width={20} color="white" />}
                  onClick={handleAddStockData}
                >
                  Add Stock Detail
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type={warning === true ? 'error' : 'success'}
      />
      <CustomSidePopup
        title={'Add Stock Details'}
        description={'Manage the stock details of the project'}
        open={open}
        handleClose={handleClosePopup}
        width={'70%'}
        content={
          <ProjectStockAdd
            setOpen={setOpen}
            open={open}
            project_id={projectId}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
            reload={reload}
            setReload={setReload}
            siteId={filterValue === null ? Number(initialSiteId) : filterValue}
          />
        }
      />
    </div>
  );
};

export default ProjectStockmanagement;
