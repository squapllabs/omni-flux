import React from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import CustomCard from '../ui/CustomCard';
import BackArrow from '../menu/icons/backArrow';
import { useGetOnePurchaseOrder } from '../../hooks/purchase-request-hooks';
import { getByProjectId } from '../../hooks/project-hooks';
import Styles from '../../styles/billView.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';

const BillView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const order_id = Number(routeParams?.id);
  const { data: getOneBillDetail } = useGetOnePurchaseOrder(order_id);
  const project_id = Number(
    getOneBillDetail?.purchase_request_data?.project_id
  );
  const { data: getOneProject } = getByProjectId(project_id);
  const generateCustomQuotationName = () => {
    const vendorName = getOneBillDetail?.vendor_data?.vendor_name || '';
    const year = new Date().getFullYear();
    const customBillName = `ALM-${vendorName.substring(0, 5)}-${year}`;
    return customBillName.toUpperCase();
  };
  const generateCustomBillName = () => {
    const vendorName = getOneBillDetail.vendor_data?.vendor_name || '';
    const projectName = getOneProject?.project_name || '';
    const year = new Date().getFullYear();
    const customBillName = `ALM-${projectName.substring(
      0,
      3
    )}-${vendorName.substring(0, 3)}-${year}`;
    return customBillName.toUpperCase();
  };
  const cost = formatBudgetValue(Number(getOneBillDetail?.total_cost));

  return (
    <div>
      <div className={Styles.title}>
        <h2>Payment Information</h2>
        <Button
          type="button"
          color="primary"
          shape="rectangle"
          size="small"
          justify="center"
          icon={<BackArrow />}
          onClick={() => navigate('/finance-view')}
        >
          Back
        </Button>
      </div>
      <div className={Styles.cardContent}>
        <CustomCard>
          <div className={Styles.mainContent}>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Project Name</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.project_name
                  ? `${getOneProject?.project_name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Vendor Name</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneBillDetail?.vendor_data?.vendor_name
                  ? `${getOneBillDetail?.vendor_data?.vendor_name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Cost</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneBillDetail?.total_cost ? cost : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Status</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneBillDetail?.status
                  ? `${getOneBillDetail?.status}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Paymet Mode</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneBillDetail?.payment_mode
                  ? `${getOneBillDetail?.payment_mode}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Paymet Date</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneBillDetail?.payment_date
                  ? `${format(
                      new Date(getOneBillDetail?.payment_date),
                      'MMM dd, yyyy'
                    )}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Bill</div>
              <div className={Styles.rightData}>
                <ol className={Styles.siteList}>
                  {getOneBillDetail?.purchase_order_documents.length > 0 ? (
                    getOneBillDetail?.purchase_order_documents.map(
                      (document: any, index: any) => (
                        <li key={index}>
                          <a
                            href={document.path}
                            target="_blank"
                            rel="noopener noreferrer"
                          >
                            {generateCustomBillName()}
                          </a>
                        </li>
                      )
                    )
                  ) : (
                    <li>-</li>
                  )}
                </ol>
              </div>
            </div>
          </div>
        </CustomCard>
      </div>
    </div>
  );
};
export default BillView;
