import Styles from '../../../../styles/myOrders.module.scss';
import React, { useState, useEffect } from 'react';
import { useGetAllMyOrderData } from '../../../../hooks/purchase-request-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import CustomLoader from '../../../ui/customLoader';
import ViewIcon from '../../../menu/icons/viewIcon';
import { formatBudgetValue } from '../../../../helper/common-function';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import { useGetProjectSite } from '../../../../hooks/project-hooks';
import CustomPagination from '../../../menu/CustomPagination';
import OrderIcon from '../../../menu/icons/orderIcon';
import CustomGroupButton from '../../../ui/CustomGroupButton';
import ExpandIcon from '../../../menu/icons/expandIcon';
import ExpandClose from '../../../menu/icons/expandClose';
import { format } from 'date-fns';
import AddIcon from '../../../menu/icons/addIcon';

const MyOrderList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const projectId = Number(routeParams?.id);
  const [poID, setPoID] = useState<any>({});

  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [activeButton, setActiveButton] = useState<string | null>(
    'Head Office'
  );
  const [buttonLabels, SetButtonLabels] = useState([
    { label: 'Head Office', value: 'Head Office' },
    { label: 'Local Purchase', value: 'Local Purchase' },
  ]);
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const [colps, setColps] = useState(false);

  const { data: getSiteList, isLoading: siteLoading } = useGetProjectSite(
    Number(projectId)
  );

  const [selectedValue, setSelectedValue] = useState('');

  const handleExpand = async (data: any) => {
    setColps(!colps);
    if (colps === true) {
      setPoID(data);
    } else {
      setPoID({});
    }
  };

  const getPoData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'asc',
    status: 'AC',
    global_search: '',
    project_id: projectId,
    site_id: selectedValue,
    purchase_order_type: activeButton,
  };

  const {
    isLoading: dataLoading,
    data: getAllData,
    refetch,
  } = useGetAllMyOrderData(getPoData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, selectedValue, activeButton]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd-MM-yyyy');
    return formattedDate;
  };

  return (
    <div>
      <CustomLoader loading={dataLoading} size={48}>
        <div>
          <div className={Styles.topHeading}>
            <div className={Styles.heading}>
              <div className={Styles.headingOne}>
                <div className={Styles.subHeading}>
                  <OrderIcon />
                  <h3>Stock Movements</h3>
                </div>
              </div>
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
              <div className={Styles.searchBar}>
                <AutoCompleteSelect
                  name="site_id"
                  value={getSiteList}
                  placeholder="Select Site"
                  width="250px"
                  onSelect={(value) => {
                    if (value !== null) {
                      setSelectedValue(value);
                    }
                  }}
                  optionList={getSiteList != null ? getSiteList : []}
                  showClearIcon={true}
                />
              </div>
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th></th>
                    <th className={Styles.tableHeading}>#</th>
                    <th className={Styles.tableHeading}>Order Id</th>
                    <th className={Styles.tableHeading}>Site Name</th>
                    <th className={Styles.tableHeading}>Total Cost</th>
                    {activeButton === 'Head Office' ? (
                      <th className={Styles.tableHeading}>Selected Vendor</th>
                    ) : (
                      ''
                    )}

                    <th className={Styles.tableHeading}>Actions</th>
                  </tr>
                </thead>
                <tbody>
                  {getAllData?.content?.length > 0 ? (
                    getAllData?.content?.map((data: any, index: any) => {
                      return (
                        <>
                          <tr key={data?.purchase_order_id}>
                            <td>
                              <div
                                onClick={() => {
                                  handleExpand(data);
                                }}
                                style={{
                                  display:
                                    data?.grn?.length !== 0 ? '' : 'none',
                                }}
                              >
                                {colps === false &&
                                data?.purchase_order_id ===
                                  poID?.purchase_order_id ? (
                                  <ExpandClose />
                                ) : (
                                  <ExpandIcon />
                                )}
                              </div>
                            </td>
                            <td>{startingIndex + index}</td>
                            <td>{data?.order_id}</td>
                            {activeButton === 'Head Office' ? (
                              <td>
                                {data?.purchase_request_data?.site_data?.name}
                              </td>
                            ) : (
                              <td>
                                {data?.indent_request_data?.site_data?.name}
                              </td>
                            )}
                            <td>{formatBudgetValue(data?.total_cost)}</td>
                            {activeButton === 'Head Office' ? (
                              <td>
                                {
                                  data?.purchase_request_data
                                    ?.selected_vendor_data?.vendor_name
                                }
                              </td>
                            ) : (
                              ''
                            )}
                            <td>
                              <AddIcon
                                color="#7f56d9"
                                onClick={() => {
                                  navigate(
                                    `/delivery-note/${data.purchase_order_id}`,
                                    { state: { projectId } }
                                  );
                                }}
                              />
                            </td>
                          </tr>
                          {data.purchase_order_id ===
                            poID.purchase_order_id && (
                            <tr key={data?.purchase_order_id}>
                              <td colSpan="8" style={{ paddingRight: 28 }}>
                                <div className={Styles.subTableContainer}>
                                  <table
                                    className={Styles.scrollable_sub_table}
                                  >
                                    <thead>
                                      <tr>
                                        <th>S No</th>
                                        <th>Goods Received Date</th>
                                        <th>Invoice No</th>
                                        <th>Options</th>
                                      </tr>
                                    </thead>
                                    <tbody>
                                      {data?.grn?.map(
                                        (grn_data: any, index: any) => {
                                          return (
                                            <tr key={grn_data?.grn_id}>
                                              <td>{index + 1}</td>{' '}
                                              <td>
                                                {dateFormat(
                                                  grn_data?.goods_received_date
                                                )}
                                              </td>
                                              <td>{grn_data?.invoice_id}</td>
                                              <td>
                                                <ViewIcon
                                                  onClick={() => {
                                                    navigate(
                                                      `/view-received-goods/${data?.purchase_order_id}/${grn_data?.grn_id}`,
                                                      { state: { projectId } }
                                                    );
                                                  }}
                                                />
                                              </td>
                                            </tr>
                                          );
                                        }
                                      )}
                                    </tbody>
                                  </table>
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
                        No data
                      </td>
                    </tr>
                  )}
                </tbody>
              </table>
            </div>
            <div>
              <CustomPagination
                currentPage={currentPage}
                totalPages={getAllData?.total_page}
                totalCount={getAllData?.total_count}
                rowsPerPage={rowsPerPage}
                onPageChange={handlePageChange}
                onRowsPerPageChange={handleRowsPerPageChange}
              />
            </div>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default MyOrderList;
