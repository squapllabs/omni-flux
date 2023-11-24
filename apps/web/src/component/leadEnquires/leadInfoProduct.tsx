import { useParams } from 'react-router-dom';
import CustomCard from '../ui/CustomCard';
import { useGetByleadEnquiryID } from '../../hooks/leadEnquires-hooks';
import Styles from '../../styles/projectInfo.module.scss';
import Button from '../ui/button';
import { useNavigate } from 'react-router-dom';

const LeadInfoProduct = () => {
  const routeParams = useParams();
  const LeadId = Number(routeParams?.id);
  const { data: getOneLead } = useGetByleadEnquiryID(LeadId);

  const navigate = useNavigate();
  return (
    <div>
      <div className={Styles.title}>
        <h2>Lead Product Information</h2>
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
                {''}
                {getOneLead?.client_contact_name
                  ? `${getOneLead?.client_contact_name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Approximate Value</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_enquiry_product?.[0]?.approx_value
                  ? `${getOneLead?.lead_enquiry_product?.[0]?.approx_value}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Sales Person Name</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneLead?.lead_enquiry_product?.[0]?.sales_person_details
                  ?.first_name
                  ? `${
                      getOneLead?.lead_enquiry_product?.[0]
                        ?.sales_person_details?.first_name
                    }
                    ${
                      getOneLead?.lead_enquiry_product?.[0]
                        ?.sales_person_details?.last_name
                        ? getOneLead?.lead_enquiry_product?.[0]
                            ?.sales_person_details?.last_name
                        : ''
                    } `
                  : 'Not Provided'}
              </div>
            </div>
          </div>
        </CustomCard>
        <div className={Styles.dataRows}>
          <table>
            <thead>
              <tr>
                <th>S No</th>
                <th>Product</th>
                <th>Quantity</th>
              </tr>
            </thead>
            <tbody>
              {getOneLead?.lead_enquiry_product?.[0]?.lead_enquiry_product_item
                .length === 0 ? (
                <tr>
                  <td></td>
                  <td>No data found</td>
                  <td></td>
                </tr>
              ) : (
                ''
              )}
              {getOneLead?.lead_enquiry_product?.[0]?.lead_enquiry_product_item.map(
                (product: any, index: number) => (
                  <tr>
                    <td>{index + 1}</td>
                    <td>{product.product?.item_name}</td>
                    <td>{product.quantity}</td>
                  </tr>
                )
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
};

export default LeadInfoProduct;
