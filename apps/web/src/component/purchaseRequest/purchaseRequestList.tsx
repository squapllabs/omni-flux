import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/purchaseRequest.module.scss';
import ProjectSubheader from '../project/projectSubheader';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import { useNavigate, useParams } from 'react-router-dom';
import { getBySearchPR } from '../../hooks/purchase-request-hooks';
import { format } from 'date-fns';
import Button from '../ui/Button';
import DownloadIcon from '../menu/icons/pdfDownloadIcon';
import ReportGenerator from '../reportGenerator/pdfReport/requestForQuotation';
import PrintIcon from '../menu/icons/printIcon';

const PurchaseRequestList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  console.log('routeParams', routeParams?.id);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(50);
  const purchaseData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'asc',
    status: 'AC',
    global_search: '',
    indent_request_id: Number(routeParams?.id),
    purchase_request_status: '',
  };
  const {
    data: getPRbasedOnIndent,
    isLoading: loading,
    refetch,
  } = getBySearchPR(purchaseData);
  console.log('getPRbasedOnIndent', getPRbasedOnIndent);
  useEffect(() => {
    refetch();
  }, []);
  const { data: getAllmasterDataForDrop = [] } = useGetAllProjectDrop();
  const [filterValue, setFilterValues] = useState<any>({
    project_id: '',
  });
  const handleReportGenerator = async (data: any) => {
    await ReportGenerator(data);
  };
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd MMM yyyy');
    return formattedDate;
  };
  const handleChange = () => {};
  const handleQuotation = (value: any) => {
    console.log('handleQuotation', value);
    navigate(`/vendor-select/${Number(value?.purchase_request_id)}`, {
      state: {
        project_id: value.project_data.project_id,
        indent_id: value?.indent_request_data?.indent_request_id,
      },
    });
  };
  return (
    <div className={Styles.container}>
      <div>
        <ProjectSubheader
          title="Purchase Request List"
          navigation="/purchase-view"
          description="List of purchase request on every project"
        />
      </div>
      <div>
        {/* <div className={Styles.searchField}>
          <div className={Styles.inputFilter}>
            <AutoCompleteSelect
              name="project_id"
              label="Select Project"
              defaultLabel="Select from options"
              placeholder="Select from options"
              value={filterValue.project_id}
              onChange={() => handleChange()}
              onSelect={(value) => {
                setFilterValues({ ...filterValue, ['project_id']: value });
                //   setIsResetDisabled(false);
              }}
              optionList={getAllmasterDataForDrop}
            />
            <Input
              width="260px"
              prefixIcon={<SearchIcon />}
              label="Indent Code"
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
              placeholder="Search by Code"
            />
          </div>
        </div> */}
        <div className={Styles.cardBox}>
          {getPRbasedOnIndent?.content?.length === 0 && (
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                textAlign: 'center',
              }}
            >
              NO PR Raised
            </div>
          )}
          {getPRbasedOnIndent?.content?.map((items: any, index: number) => {
            console.log('items', items?.purchase_order?.length);

            return (
              <div className={Styles.cardContainer}>
                <div className={Styles.cardHeadpanelOne}>
                  {/* <div>PR - {index + 1}</div> */}
                  <div className={Styles.panelContent}>
                    <span className={Styles.panelContentTitle}>PR Code:</span>
                    {items?.purchase_request_code}
                  </div>
                  <div className={Styles.panelContent}>
                    <span className={Styles.panelContentTitle}>
                      PR raised Date :
                    </span>
                    {dateFormat(
                      items?.request_date ? items?.request_date : new Date()
                    )}
                  </div>
                  <div className={Styles.panelContent}>
                    {/* <div> */}
                    <span className={Styles.panelContentTitle}>Status:</span>
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
                      {items?.status}
                    </span>
                    {/* </div> */}
                  </div>
                </div>
                {/* <div className={Styles.dividerStyle}></div> */}
                <div className={Styles.cardpanelOne}>
                  <div className={Styles.panelContent}>
                    <span className={Styles.panelContentTitle}>
                      Indent Code :
                    </span>
                    {items?.indent_request_data?.indent_request_code}
                  </div>
                  <div className={Styles.panelContent}>
                    <span className={Styles.panelContentTitle}>
                      No of Items :
                    </span>
                    {items?.vendor_quotes[0]?.vendor_quotation_details?.length}
                  </div>
                  <div className={Styles.panelContent}>
                    <span className={Styles.panelContentTitle}>
                      Expected Delivery Date :
                    </span>
                    {dateFormat(
                      items?.indent_request_data?.expected_delivery_date
                        ? items?.indent_request_data?.expected_delivery_date
                        : new Date()
                    )}
                  </div>
                </div>
                <div className={Styles.cardpanelTwo}>
                  <div className={Styles.panelContent}>
                    <span className={Styles.panelContentTitle}>Vendors :</span>
                    <div className={Styles.vendorPanel}>
                      {items?.vendor_quotes?.map(
                        (vendors: any, vendorIndex: number) => {
                          return (
                            <ol>
                              <li
                                className={`${Styles.status} ${
                                  items?.selected_vendor_data?.vendor_id ===
                                  vendors?.vendor_data?.vendor_id
                                    ? Styles.completedStatus
                                    : ''
                                }`}
                              >
                                <div>{vendorIndex + 1}</div>
                                {vendors?.vendor_data?.vendor_name}
                              </li>
                            </ol>
                          );
                        }
                      )}
                    </div>
                  </div>
                  <div>
                    <div className={Styles.panelContent}>
                      <span className={Styles.panelContentTitle}>
                        Site Name :
                      </span>
                      <div>{items?.site_data?.name}</div>
                    </div>
                  </div>
                </div>
                <div className={Styles.cardpanelThree}>
                  <div
                  // style={{
                  //   display: items?.status === 'Approved' ? 'none' : '',
                  // }}
                  >
                    <Button
                      shape="rectangle"
                      justify="center"
                      size="small"
                      color="primary"
                      onClick={() => handleQuotation(items)}
                    >
                      {items?.status === 'Approved' ? 'View' : 'Add Quotation'}
                    </Button>
                  </div>
                  <div
                    style={{
                      display: items?.selected_vendor_id === null ? 'none' : '',
                    }}
                  >
                    <Button
                      shape="rectangle"
                      justify="center"
                      size="small"
                      color="primary"
                      disabled={
                        items?.purchase_order?.length === 0 ? false : true
                      }
                      onClick={() => {
                        navigate(
                          `/purchase-request/${items?.purchase_request_id}`
                        );
                      }}
                    >
                      Convert to PO
                    </Button>
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
      </div>
    </div>
  );
};

export default PurchaseRequestList;
