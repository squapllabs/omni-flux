import Styles from '../../../../styles/myOrders.module.scss'
import BOQIcon from '../../../menu/icons/boqIcon';
import React, { useState, useEffect } from 'react';
import Select from '../../../ui/selectNew';
import {
    useGetAllPurchaseOrderData,
} from '../../../../hooks/purchase-request-hooks';
import { useGetAllGrnData } from '../../../../hooks/grn-hooks';
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
import ExpandIcon from '../../../menu/icons/expandIcon';
import ExpandClose from '../../../menu/icons/expandClose';
import { format } from 'date-fns';
import AddDataIcon from '../../../menu/icons/addDataIcon';

const MyOrderList = () => {
    const routeParams = useParams();
    const navigate = useNavigate();
    const projectId = Number(routeParams?.id);
    const [poID, setPoID] = useState<any>({});

    const [currentPage, setCurrentPage] = useState(1);
    const [rowsPerPage, setRowsPerPage] = useState(10);
    const [activeButton, setActiveButton] = useState<string | null>('Head Office');
    const [buttonLabels, SetButtonLabels] = useState([
        { label: 'Head Office', value: 'Head Office' },
        { label: 'Local Purchase', value: 'Local Purchase' }
    ])
    const handleGroupButtonClick = (value: string) => {
        setActiveButton(value);
    };
    const [colps, setColps] = useState(false);

    let rowIndex = 0;

    const { data: getSiteList, isLoading: siteLoading } = getProjectSite(Number(projectId));

    const [selectedValue, setSelectedValue] = useState('');

    const handleExpand = async (data: any) => {
        setColps(!colps);
        if (colps === true) {
            setPoID(data);
        }
        else {
            setPoID({});
        }

    };

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
        purchase_order_type: activeButton
    };

    const {
        isLoading: dataLoading,
        data: getAllData,
        refetch,
    } = useGetAllPurchaseOrderData(getPoData);

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

    let separateArray: any = [];
    getAllData?.content?.forEach((item: any) => {
        // Check if item.grn exists and is an array
        if (Array.isArray(item?.grn)) {
            // Use forEach or map to process each data element in item.grn
            item.grn.forEach((data: any) => {
                separateArray.push(data);
            });
            return separateArray;
        }
    });

    console.log("gd>>", getAllData);

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
                                            <th className={Styles.tableHeading}>Order Remark</th>
                                            <th className={Styles.tableHeading}>Site Name</th>
                                            <th className={Styles.tableHeading}>Total Cost</th>
                                            <th className={Styles.tableHeading}>Selected Vendor</th>
                                            <th className={Styles.tableHeading}>Actions</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {getAllData?.total_count !== 0 ? (
                                            getAllData?.content?.map((data: any, index: any) => {
                                                // console.log("DAta>>>", data);

                                                return (
                                                    <>
                                                        <tr key={data.purchase_order_id}>
                                                            <td>
                                                                <div
                                                                    onClick={() => {
                                                                        handleExpand(data);
                                                                    }}
                                                                    style={{
                                                                        display:
                                                                            data?.grn?.length !== 0
                                                                                ? ''
                                                                                : 'none',
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
                                                                <AddDataIcon onClick={() => {
                                                                    navigate(
                                                                        `/delivery-note/${data.purchase_order_id}`,
                                                                        { state: { projectId } }
                                                                    );
                                                                }} />
                                                            </td>
                                                        </tr>
                                                        {data.purchase_order_id ===
                                                            poID.purchase_order_id && (
                                                                <tr>
                                                                    <td colSpan="8" style={{ paddingRight: 28 }}>
                                                                        <div className={Styles.subTableContainer}>
                                                                            <table className={Styles.scrollable_table}>
                                                                                <thead>
                                                                                    <tr>
                                                                                        <th>S No</th>
                                                                                        <th>Goods Received Date</th>
                                                                                        <th>Invoice No</th>
                                                                                        <th>Options</th>
                                                                                    </tr>
                                                                                </thead>
                                                                                <tbody>
                                                                                    {
                                                                                        data?.grn?.map((gnr_data: any, index: any) => {
                                                                                            // console.log("gnr_data///", gnr_data);
                                                                                            return (
                                                                                                <tr key={gnr_data?.grn_id}>
                                                                                                    <td>{index + 1}</td> {/* Use 'index' from the outer map */}
                                                                                                    <td>{dateFormat(gnr_data?.goods_received_date)}</td>
                                                                                                    <td>{gnr_data?.invoice_id}</td>
                                                                                                    {/* {getAllData?.} */}
                                                                                                    <td>
                                                                                                        <ViewIcon
                                                                                                            onClick={() => {
                                                                                                                navigate(`/view-received-goods/${data?.purchase_order_id}/${gnr_data?.grn_id}`,
                                                                                                                    { state: { projectId } }
                                                                                                                );
                                                                                                            }}
                                                                                                        />
                                                                                                    </td>
                                                                                                </tr>
                                                                                            )
                                                                                        })
                                                                                    }
                                                                                </tbody>
                                                                            </table>
                                                                        </div>

                                                                    </td>
                                                                </tr>
                                                            )}
                                                    </>
                                                )
                                            })
                                        ) : (
                                            <tr>
                                                <td colSpan="6" style={{ textAlign: 'center' }}>
                                                    No data found
                                                </td>
                                            </tr>

                                        )}

                                        {/* );
                                        )} */}
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
                )
                }
            </CustomLoader >
        </div >
    );
};

export default MyOrderList;
