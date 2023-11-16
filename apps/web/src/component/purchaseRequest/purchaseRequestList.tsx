import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/purchaseRequest.module.scss';
import ProjectSubheader from '../project/projectSubheader';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
// import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import { Link, useNavigate, useParams } from 'react-router-dom';
import { useGetBySearchPR } from '../../hooks/purchase-request-hooks';
import { format } from 'date-fns';
// import Button from '../ui/Button';
// import DownloadIcon from '../menu/icons/pdfDownloadIcon';
import ReportGenerator from '../reportGenerator/pdfReport/requestForQuotation';
import PrintIcon from '../menu/icons/printIcon';
import SiteNavigateIcon from '../menu/icons/siteNavigateIcon';
import CustomLoader from '../ui/customLoader';
import CustomGroupButton from '../ui/CustomGroupButton';
import { useGetByIndnetId } from '../../hooks/indent-approval-hooks';

const PurchaseRequestList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'All', value: '' },
    { value: 'Approved', label: 'Quotation Recieved' },
    { label: 'waiting for quotation', value: 'Waiting For Quotation' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [filterValue, setFilterValues] = useState<any>({
    project_id: '',
    search_by_code: '',
  });
  const purchaseData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'asc',
    status: 'AC',
    global_search: '',
    indent_request_id: Number(routeParams?.id),
    purchase_request_status: activeButton,
    purchase_request_code: filterValue?.search_by_code,
  };
  const {
    data: getPRbasedOnIndent,
    isLoading: loading,
    refetch,
    isFetched,
  } = useGetBySearchPR(purchaseData);

  const { data: getOneIndnetData } = useGetByIndnetId(Number(routeParams?.id));

  const { data: getAllmasterDataForDrop = [] } = useGetAllProjectDrop();

  const handleReportGenerator = async (data: any) => {
    await ReportGenerator(data);
  };
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd MMM yyyy');
    return formattedDate;
  };
  const handleQuotation = (value: any) => {
    navigate(`/vendor-select/${Number(value?.purchase_request_id)}`, {
      state: {
        project_id: value.project_data.project_id,
        indent_id: value?.indent_request_data?.indent_request_id,
      },
    });
  };
  const handleVendor = (value: any, vendor: any) => {
    navigate(`/vendor-quotes-update/${Number(value?.purchase_request_id)}`, {
      state: {
        project_id: value.project_data.project_id,
        indent_id: value?.indent_request_data?.indent_request_id,
        vendor_quotes_id: vendor?.vendor_quotes_id,
        vendor_id: vendor?.vendor_id,
        vendor: vendor,
      },
    });
  };
  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValue]);
  useEffect(() => {
    refetch();
  }, [activeButton]);
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
    setCurrentPage(1);
  };
  return (
    <div className={Styles.container}>
      <CustomLoader loading={loading || !isFetched} size={30}>
        <div>
          <ProjectSubheader
            title="Purchase Request List"
            navigation="/approved-indent-list"
            description="List of purchase request on every project"
          />
        </div>
        <div>
          <div className={Styles.sub_header}>
            <div style={{ display: 'flex' }}>
              <div
                style={{
                  display: 'flex',
                  alignItems: 'center',
                  padding: '20px 10px 20px 30px',
                }}
              >
                <div className={Styles.textContent_1}>
                  <span className={Styles.projectTitle}>Indent Code</span>
                  <h3>{getOneIndnetData?.indent_request_code}</h3>
                </div>
              </div>
              <div className={Styles.lineStyles}>
                <div className={Styles.vertical}>
                  <div className={Styles.verticalLine}></div>
                </div>
              </div>
              <div
                style={{
                  display: 'flex',
                  alignItems: 'center',
                  gap: '10px',
                  padding: '20px 10px 20px 10px',
                }}
              >
                <div>
                  <SiteNavigateIcon width={30} height={30} />
                </div>
                <div className={Styles.textContent_1}>
                  <span className={Styles.projectTitle}>Site</span>
                  <span style={{ width: '160px' }}>
                    {getOneIndnetData?.site_data?.name}
                  </span>
                </div>
              </div>
              <div className={Styles.lineStyles}>
                <div className={Styles.vertical}>
                  <div className={Styles.verticalLine}></div>
                </div>
              </div>
              <div
                style={{
                  display: 'flex',
                  alignItems: 'center',
                  gap: '10px',
                  padding: '20px 10px 20px 10px',
                }}
              >
                <div className={Styles.textContent_1}>
                  <span>{getOneIndnetData?.project_data?.project_name}</span>
                </div>
              </div>
              <div className={Styles.lineStyles}>
                <div className={Styles.vertical}>
                  <div className={Styles.verticalLine}></div>
                </div>
              </div>
              <div></div>
            </div>
          </div>
          <div className={Styles.selected}></div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
              <div>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  // label="PR Code"
                  name="search_by_code"
                  value={filterValue.search_by_code}
                  onChange={(e) => {
                    setFilterValues({
                      ...filterValue,
                      ['search_by_code']: e.target.value,
                    });
                    //   setCurrentPage(1);
                    //   setIsResetDisabled(false);
                  }}
                  placeholder="Search by PR Code"
                />
              </div>
            </div>
          </div>
        </div>
        <div></div>
        {/* <div> */}
        {getPRbasedOnIndent?.content?.length === 0 && !loading ? (
          <div className={Styles.emptyDataHandling}>
            <div className={Styles.image}>
              <img src="/boq-add.png" width="100%" height="150px" />
            </div>
            <div>
              <h5 className={Styles.textmax}>
                No Purchase request is related to your search
              </h5>
            </div>
          </div>
        ) : (
          <div className={Styles.cardBox}>
            {getPRbasedOnIndent?.content?.map((items: any, index: number) => {
              return (
                <div className={Styles.cardContainer} key={index}>
                  <div>
                    <div>
                      <span>#{index + 1}</span>
                    </div>
                  </div>
                  <div className={Styles.Colps}>
                    <div className={Styles.ColpsChilds}>
                      <div className={Styles.ColpsheadingpanelOne}>
                        <span className={Styles.panelContentTitle}>
                          PR Code:
                        </span>
                        {/* <span className={Styles.panelContentTitle}>
                        Indent Code :
                      </span> */}
                        {/* <span className={Styles.panelContentTitle}>
                        PO Code :
                      </span> */}
                        <span className={Styles.panelContentTitle}>
                          Vendors :
                        </span>
                      </div>
                      <div className={Styles.ColpsDatapanelOne}>
                        <span>
                          {items?.status === 'Approved' ? (
                            <div
                              onClick={() => handleQuotation(items)}
                              className={Styles.hyperLinks}
                            >
                              <a>{items?.purchase_request_code}</a>
                            </div>
                          ) : (
                            items?.purchase_request_code
                          )}
                        </span>
                        {/* <span>
                        {items?.indent_request_data?.indent_request_code}
                      </span> */}
                        {/* <span>
                        {items?.purchase_order?.length === 0 ? (
                          'N/A'
                        ) : (
                          <a
                            href={`/purchase-order-view/${items?.purchase_order[0]?.purchase_order_id}`}
                          >
                            {items?.purchase_order[0]?.order_id}
                          </a>
                        )}
                      </span> */}
                        <div className={Styles.vendorPanel}>
                          <ol>
                            {items?.vendor_quotes?.map(
                              (vendors: any, vendorIndex: number) => {
                                return (
                                  <li
                                    key={vendorIndex}
                                    className={`${Styles.vendorLinks} ${
                                      items?.selected_vendor_data?.vendor_id ===
                                      vendors?.vendor_data?.vendor_id
                                        ? Styles.completedStatus
                                        : ''
                                    }`}
                                    onClick={() => handleVendor(items, vendors)}
                                  >
                                    <div>{vendorIndex + 1}</div>
                                    <span>
                                      {vendors?.vendor_data?.vendor_name}
                                    </span>
                                    {/* {vendors?.quotation_status === 'Approved' ? (
                                    'Approved'
                                  ) : vendors?.quotation_status ===
                                    'Pending' ? (
                                    <div className={Styles.hyperLinks}>
                                      Add Quotation
                                    </div>
                                  ) : vendors?.quotation_status ===
                                    'Quotation Recived' ? (
                                    <div className={Styles.hyperLinks}>
                                      View
                                    </div>
                                  ) : (
                                    ''
                                  )} */}
                                    <span
                                      className={`${Styles.status} ${
                                        vendors?.quotation_status === 'Approved'
                                          ? Styles.completedStatus
                                          : vendors?.quotation_status ===
                                            'Pending'
                                          ? Styles.inprogressStatus
                                          : vendors?.quotation_status ===
                                            'Rejected'
                                          ? Styles.rejectedStatus
                                          : ''
                                      }`}
                                    >
                                      {vendors?.quotation_status}
                                    </span>
                                  </li>
                                );
                              }
                            )}
                          </ol>
                        </div>
                      </div>
                    </div>
                    <div className={Styles.ColpsChilds}>
                      <div className={Styles.ColpsheadingpanelOne}>
                        <span className={Styles.panelContentTitle}>
                          PR raised Date :
                        </span>
                        <span className={Styles.panelContentTitle}>
                          No of Items :
                        </span>
                        <span className={Styles.panelContentTitle}></span>
                      </div>
                      <div className={Styles.ColpsDatapanelOne}>
                        <span>
                          {' '}
                          {dateFormat(
                            items?.request_date
                              ? items?.request_date
                              : new Date()
                          )}
                        </span>
                        <span>
                          {
                            items?.vendor_quotes[0]?.vendor_quotation_details
                              ?.length
                          }
                        </span>
                        <span></span>
                      </div>
                    </div>
                    <div className={Styles.ColpsChilds}>
                      <div className={Styles.ColpsheadingpanelOne}>
                        <span className={Styles.panelContentTitle}>
                          Status :
                        </span>
                        <span className={Styles.panelContentTitle}>
                          Requested Delivery Date :
                        </span>
                        {/* <span className={Styles.panelContentTitle}>
                        {' '}
                        Site Name :
                      </span> */}
                      </div>
                      <div className={Styles.ColpsDatapanelOne}>
                        <span
                          className={`${
                            items?.status === 'Waiting For Quotation'
                              ? Styles.inprogressStatus
                              : items?.status === 'Approved'
                              ? Styles.completedStatus
                              : ''
                          }`}
                          // style={{ padding: '6px' }}
                        >
                          {items?.status === 'Approved' &&
                          items?.purchase_order?.length === 0
                            ? 'Quotation Received'
                            : items?.status != 'Approved'
                            ? items?.status
                            : items?.status === 'Approved' &&
                              items?.purchase_order?.length > 0
                            ? 'Moved to PO'
                            : ''}
                        </span>
                        <span>
                          {dateFormat(
                            items?.indent_request_data?.expected_delivery_date
                              ? items?.indent_request_data
                                  ?.expected_delivery_date
                              : new Date()
                          )}
                        </span>
                        {/* <span>{items?.site_data?.name}</span> */}
                      </div>
                    </div>
                  </div>

                  <div className={Styles.cardpanelThree}>
                    <div
                      onClick={() => handleQuotation(items)}
                      style={{
                        display:
                          items?.status === 'Waiting For Quotation'
                            ? ''
                            : 'none',
                      }}
                    >
                      <a>Select Vendor</a>
                    </div>
                    <div
                      style={{
                        display:
                          items?.status === 'Approved' &&
                          items?.purchase_order?.length > 0
                            ? ''
                            : 'none',
                      }}
                    >
                      <Link
                        to={`/purchase-order-view/${items?.purchase_order[0]?.purchase_order_id}`}
                        // href={`/purchase-order-view/${items?.purchase_order[0]?.purchase_order_id}`}
                      >
                        View PO
                      </Link>
                    </div>
                    <div
                      style={{
                        display:
                          items?.selected_vendor_id === null ? 'none' : '',
                      }}
                    >
                      <div
                        style={{
                          display:
                            items?.purchase_order?.length === 0 ? '' : 'none',
                        }}
                      >
                        <Link
                          to={`/purchase-request/${items?.purchase_request_id}`}
                        >
                          Convert to PO
                        </Link>
                      </div>
                    </div>
                    <div style={{ paddingTop: '8px' }}>
                      <div onClick={() => handleReportGenerator(items)}>
                        <PrintIcon color="#7f56d9" />
                      </div>
                    </div>
                  </div>
                  <div className={Styles.dividerStyle}></div>
                </div>
              );
            })}
          </div>
        )}
      </CustomLoader>
    </div>
    // </div>
  );
};

export default PurchaseRequestList;
