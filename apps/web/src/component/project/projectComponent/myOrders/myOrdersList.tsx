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

const MyOrderList = () => {
    const routeParams = useParams();
    const navigate = useNavigate();
    const projectId = Number(routeParams?.id);

    const [currentPage, setCurrentPage] = useState(1);
    const [rowsPerPage, setRowsPerPage] = useState(10);
    let rowIndex = 0;

    const getPoData = {
        limit: rowsPerPage,
        offset: (currentPage - 1) * rowsPerPage,
        order_by_column: 'updated_date',
        order_by_direction: 'desc',
        status: 'AC',
        global_search: '',
        bill_status: 'Processing',
        project_id: projectId,
    };
    const {
        isLoading: dataLoading,
        data: getAllData,
        refetch,
    } = useGetAllPurchaseOrderData(getPoData);


    return (
        <div>
            <CustomLoader loading={dataLoading} size={48}>
                <div className={Styles.topHeading}>
                    <div className={Styles.heading}>
                        <div className={Styles.headingOne}>
                            <div className={Styles.subHeading}>
                                <BOQIcon />
                                <h3>My Orders</h3>
                            </div>
                        </div>
                        <div className={Styles.searchBar} style={{ gap: '10px' }}>
                            <Select
                                width="200px"
                                name="approver_status"
                                // value={filterValues.approver_status}
                                // onChange={(e) => handleFilterChange(e)}
                                defaultLabel="Select from options"
                                placeholder="Select All"
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
                                    <th className={Styles.tableHeading}>Actions</th>
                                </tr>
                            </thead>
                            <tbody>
                                {getAllData?.content?.map((data: any, index: number) => (
                                    <tr key={data.gst_id}>
                                        <td>{index + 1}</td>
                                        <td>{data.order_id}</td>
                                        <td>{data.order_remark}</td>
                                        <td><ViewIcon  onClick={() => navigate('/my-orders-view')}/></td></tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                </div>
            </CustomLoader>
        </div>
    );
};

export default MyOrderList;
