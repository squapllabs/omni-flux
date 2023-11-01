import Styles from '../../../../styles/myOrders.module.scss'
import BOQIcon from '../../../menu/icons/boqIcon';
import React, { useState, useEffect } from 'react';
import Select from '../../../ui/selectNew';
import {
    useGetAllPurchaseOrderData,
} from '../../../../hooks/purchase-request-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import CustomLoader from '../../../ui/customLoader';
import ViewIcon from '../../../menu/icons/viewIcon';
import { formatBudgetValue } from '../../../../helper/common-function';
import projectService from '../../../../service/project-service';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import { getProjectSite } from '../../../../hooks/project-hooks';
import CustomPagination from '../../../menu/CustomPagination';
import OrderIcon from '../../../menu/icons/orderIcon';
import CustomGroupButton from '../../../ui/CustomGroupButton';

const MyOrderList = () => {
    const routeParams = useParams();
    const navigate = useNavigate();
    const projectId = Number(routeParams?.id);
    const [currentPage, setCurrentPage] = useState(1);
    const [rowsPerPage, setRowsPerPage] = useState(10);
    const [activeButton, setActiveButton] = useState<string | null>('HO');
    const [buttonLabels, SetButtonLabels] = useState([
        { label: 'Head Office', value: 'HO' },
        { label: 'Local Purchase Order', value: 'LPO' }
    ])
    const handleGroupButtonClick = (value: string) => {
        setActiveButton(value);
    };

    let rowIndex = 0;

    const { data: getSiteList, isLoading: siteLoading } = getProjectSite(Number(projectId));

    // const initialSiteId =
    //     !siteLoading && getSiteList ? getSiteList[0]?.value : null;
    const [selectedValue, setSelectedValue] = useState('');

    const getPoData = {
        limit: rowsPerPage,
        offset: (currentPage - 1) * rowsPerPage,
        order_by_column: 'updated_date',
        order_by_direction: 'desc',
        status: 'AC',
        global_search: '',
        bill_status: 'Processing',
        project_id: projectId,
        site_id: selectedValue,
    };

    const {
        isLoading: dataLoading,
        data: getAllData,
        refetch,
    } = useGetAllPurchaseOrderData(getPoData);

    useEffect(() => {
        refetch();
    }, [currentPage, rowsPerPage, selectedValue]);

    // useEffect(() => {
    //     refetch();
    // }, [initialSiteId, !siteLoading]);

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
    // console.log("getAllData", getAllData);

    return (
        <div>
            <CustomLoader loading={dataLoading} size={48}>
                {getAllData?.is_available ? (
                    <div>
                        <div className={Styles.topHeading}>
                            <div className={Styles.heading}>
                                <div className={Styles.headingOne}>
                                    <div className={Styles.subHeading}>
                                        <OrderIcon />
                                        <h3>My Orders</h3>
                                    </div>
                                </div>
                                <div className={Styles.searchBar}>
                                    <div>
                                        <CustomGroupButton
                                            labels={buttonLabels}
                                            onClick={handleGroupButtonClick}
                                            activeButton={activeButton}
                                        />
                                    </div>
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
                                            <th className={Styles.tableHeading}>#</th>
                                            <th className={Styles.tableHeading}>Order Id</th>
                                            <th className={Styles.tableHeading}>Order Remark</th>
                                            <th className={Styles.tableHeading}>Site Name</th>
                                            <th className={Styles.tableHeading}>Total Cost</th>
                                            <th className={Styles.tableHeading}>Selected Vendor</th>
                                            <th className={Styles.tableHeading}>Actions</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {getAllData?.total_count === 0 ? (
                                            <tr>
                                                <td colSpan="8" style={{ textAlign: 'center' }}>
                                                    No data found
                                                </td>
                                            </tr>
                                        ) : (
                                            getAllData?.content?.map((data: any, index: number) => (
                                                <tr key={data.purchase_order_id}>
                                                    <td>{startingIndex + index}</td>
                                                    <td>{data?.order_id}</td>
                                                    <td>{data?.order_remark}</td>
                                                    <td>{data?.purchase_request_data?.site_data?.name}</td>
                                                    <td>{formatBudgetValue(data?.total_cost)}</td>
                                                    <td>{data?.purchase_request_data?.selected_vendor_data?.vendor_name}</td>
                                                    <td>
                                                        {/* <ViewIcon onClick={() => {
                                                            navigate(
                                                                `/my-orders-view/${data.purchase_order_id}`,
                                                                { state: { projectId } }
                                                            );
                                                        }} /> */}
                                                          <ViewIcon onClick={() => {
                                                            navigate(
                                                                `/delivery-note/${data.purchase_order_id}`,
                                                                { state: { projectId } }
                                                            );
                                                        }} />
                                                    </td>
                                                </tr>
                                            ))
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
                ) : (
                    <div>
                        <div className={Styles.subHeadingForInitialPage}>
                            <OrderIcon />
                            <h3>My Orders</h3>
                        </div>
                        <div className={Styles.emptyDataHandling}>
                            <div>
                                <img
                                    src="/vendor.jpg"
                                    alt="aa"
                                    width="100%"
                                    height="150px"
                                    style={{ paddingBottom: '15px' }}
                                />
                            </div>
                            <div>
                                <h5 className={Styles.textmax}>Orders List is Empty</h5>
                            </div>
                            <div className={Styles.contentGap}>
                                <p className={Styles.textmin}>This project has no orders to display</p>
                            </div>
                        </div>
                    </div>
                )}
            </CustomLoader>
        </div>
    );
};

export default MyOrderList;
