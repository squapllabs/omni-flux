import { useParams } from 'react-router-dom';
import CustomCard from '../ui/CustomCard';
import { useGetByleadEnquiryID } from '../../hooks/leadEnquires-hooks';
import Styles from '../../styles/projectInfo.module.scss';
import Button from '../ui/button';
import { useNavigate } from 'react-router-dom';
import { format } from 'date-fns';

const LeadInfoTender = () => {
  const routeParams = useParams();
  const LeadId = Number(routeParams?.id);
  const { data: getOneLead } = useGetByleadEnquiryID(LeadId);
  const navigate = useNavigate();
  return (
    <div>
      <div className={Styles.title}>
        <h2>Lead Tender Information</h2>
        <Button
          color="primary"
          shape="rectangle"
          justify="center"
          size="small"
          onClick={() => {
            navigate('/settings');
          }}
        >
          Back
        </Button>
        {/* <Button
          text="Back"
          backgroundColor="#7F56D9"
          fontSize={14}
          fontWeight={500}
          width={100}
          onClick={() => navigate('/settings')}
        /> */}
      </div>
      <div className={Styles.cardContent}>
        <CustomCard>
          <div className={Styles.mainContent}>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Lead Type</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_type
                  ? `${getOneLead?.lead_type}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Lead Code</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_code
                  ? `${getOneLead?.lead_code}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Lead Registration Number</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_enquiry_tenders?.[0]?.tender_reg_no
                  ? `${getOneLead?.lead_enquiry_tenders?.[0]?.tender_reg_no}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Lead Tender Id</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_enquiry_tenders?.[0]?.lead_tender_id
                  ? `${getOneLead?.lead_enquiry_tenders?.[0]?.lead_tender_id}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Tender Issue Date</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_enquiry_tenders?.[0]?.tender_issue_date
                  ? `${format(
                      new Date(
                        getOneLead?.lead_enquiry_tenders?.[0]?.tender_issue_date
                      ),
                      'MMM dd, yyyy'
                    )}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Tender Due Date</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_enquiry_tenders?.[0]?.tender_due_date
                  ? `${format(
                      new Date(
                        getOneLead?.lead_enquiry_tenders?.[0]?.tender_due_date
                      ),
                      'MMM dd, yyyy'
                    )}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Estimated Value</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_enquiry_tenders?.[0]?.estimated_value
                  ? `${getOneLead?.lead_enquiry_tenders?.[0]?.estimated_value}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Client Name</div>
              <div className={Styles.rightData}>
                {''}
                {getOneLead?.client_info?.name
                  ? `${getOneLead?.client_info?.name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Client Contact Name</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.client_contact_name
                  ? `${getOneLead?.client_contact_name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Client Contact Email</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.client_contact_email
                  ? `${getOneLead?.client_contact_email}`
                  : 'Not Provided'}
              </div>
            </div>
          </div>
        </CustomCard>
      </div>
    </div>
  );
};

export default LeadInfoTender;
