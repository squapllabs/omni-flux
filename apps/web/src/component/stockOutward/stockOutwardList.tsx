import React, { useState, useEffect } from 'react';
import Styles from '../../styles/stockOutwardList.module.scss';
import Button from '../ui/Button';
import { getAllPaginatedStockOutwardData } from '../../hooks/stock-outward';
import CustomLoader from '../ui/customLoader';
import { format } from 'date-fns';
import { useNavigate } from 'react-router';
import AddIcon from '../menu/icons/addIcon';
import { getProjectSite } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import ViewIcon from '../menu/icons/newViewIcon';
import EditIcon from '../menu/icons/newEditIcon';
import { useParams } from 'react-router-dom';
import CustomPagination from '../menu/CustomPagination';
import StoreOutIcon from '../menu/icons/storeOutIcon';

const StockOutwardList = () => {
  const navigate = useNavigate();
  const routeParams = useParams();

  const { data: getSiteList, isLoading: siteLoading } = getProjectSite(
    Number(routeParams?.id)
  );
  const initialSiteId =
    !siteLoading && getSiteList ? getSiteList[0]?.value : null;

  const [selectedValue, setSelectedValue] = useState(initialSiteId);

  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);

  const stockData: any = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    site_id: selectedValue,
    project_id: Number(routeParams?.id),
  };

  const {
    data: getStockData,
    isLoading: FilterLoading,
    refetch,
  } = getAllPaginatedStockOutwardData(stockData);

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, selectedValue]);
  useEffect(() => {
    refetch();
  }, [initialSiteId, !siteLoading]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.topHeading}>
          <div className={Styles.textContent}>
            <div className={Styles.heading}>
              <StoreOutIcon />
              <div className={Styles.content}>
                <h3>Stock Outward</h3>
              </div>
            </div>
            <div>
              <AutoCompleteSelect
                name="site_id"
                optionList={getSiteList != null ? getSiteList : []}
                value={
                  selectedValue === null && getSiteList != null
                    ? Number(initialSiteId)
                    : selectedValue
                }
                placeholder="Select Site"
                onSelect={(value) => {
                  if (value !== null) {
                    setSelectedValue(value);
                  }
                }}
                showClearIcon={false}
              />
            </div>
          </div>
          {getStockData?.total_count > 0 ? (
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon color="white" />}
                onClick={() =>
                  navigate('/stockoutward-add', {
                    state: { projectId: routeParams?.id },
                  })
                }
              >
                Add
              </Button>
            </div>
          ) : (
            ''
          )}
        </div>
        {getStockData?.total_count > 0 ? (
          <div>
            <div className={Styles.tableContainer}>
              <div>
                <table className={Styles.scrollable_table}>
                  <thead>
                    <tr>
                      <th className={Styles.tableHeading}>#</th>
                      <th className={Styles.tableHeading}>Project</th>
                      <th className={Styles.tableHeading}>Outward ID</th>
                      <th className={Styles.tableHeading}>Site</th>
                      <th className={Styles.tableHeading}>Site Engineer</th>
                      <th className={Styles.tableHeading}>Item Count</th>
                      <th className={Styles.tableHeading}>Delivery Date</th>
                      <th className={Styles.tableHeading}>Actions</th>
                    </tr>
                  </thead>
                  <tbody>
                    {getStockData?.total_count === 0 ? (
                      <tr>
                        <td colSpan="4" style={{ textAlign: 'center' }}>
                          No data found
                        </td>
                      </tr>
                    ) : (
                      ''
                    )}
                    {getStockData?.content?.map((data: any, index: number) => {
                      return (
                        <tr key={data.stock_outward_id}>
                          <td>{startingIndex + index}</td>
                          <td>{data?.project_data?.project_name}</td>
                          <td>{data?.outward_id}</td>
                          <td>{data?.site_data?.name}</td>
                          <td>
                            {data?.site_engineer_data?.first_name +
                              ' ' +
                              data?.site_engineer_data?.last_name}
                          </td>
                          <td>{data?.stock_outward_details?.length}</td>
                          <td>
                            {format(
                              new Date(data?.stock_outward_date),
                              'dd-MMM-yyyy'
                            )}
                          </td>
                          <td>
                            <div className={Styles.tablerow}>
                              <EditIcon
                                onClick={() =>
                                  navigate(
                                    `/stockoutward-edit/${data?.stock_outward_id}`,
                                    { state: { projectId: routeParams?.id } }
                                  )
                                }
                              />
                              <ViewIcon
                                onClick={() =>
                                  navigate(
                                    `/stockoutward-view/${data?.stock_outward_id}`,
                                    { state: { projectId: routeParams?.id } }
                                  )
                                }
                              />
                            </div>
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            </div>
            <div>
              <CustomPagination
                currentPage={currentPage}
                totalPages={getStockData?.total_page}
                totalCount={getStockData?.total_count}
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
                <img src="/stock.jpg" alt="aa" width="100%" height="200px" />
              </div>
              <div>
                <h5 className={Styles.textmax}>This project has no Stock Details</h5>
              </div>
              <div>
                <p className={Styles.textmin}>Manage Stock Outward details to this project now </p>
              </div>
              <div className={Styles.emptyButton}>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() =>
                    navigate('/stockoutward-add', {
                      state: { projectId: routeParams?.id },
                    })
                  }
                >
                  Add Stock Outward
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
    </div>
  );
};

export default StockOutwardList;
