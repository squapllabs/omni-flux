import React from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import Styles from '../../styles/vendorView.module.scss';
import CustomCard from '../ui/CustomCard';
import { getByVendorId } from '../../hooks/vendor-hooks';
import BackArrowIcon from '../menu/icons/backArrow';
import ProjectSubheader from '../project/projectSubheader';

const VendorView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const VendorId = Number(routeParams?.id);
  const { data: getOneVendor } = getByVendorId(VendorId);
  return (
    <div>
      {/* <div className={Styles.title}>
        <h2>Vendor Information</h2>
        <Button
          shape="rectangle"
          justify="center"
          size="small"
          color="primary"
          icon={<BackArrowIcon />}
          onClick={() => navigate('/settings')}
        >
          Back
        </Button>
      </div> */}
      <div>
        <ProjectSubheader
          title={'Vendor Information'}
          navigation={'/vendor-list'}
        />
      </div>
      <div className={Styles.cardContent}>
        <CustomCard>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor Name</div>
            <div className={Styles.rightData}>
              {getOneVendor?.vendor_name
                ? `${getOneVendor?.vendor_name}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor Contact Person</div>
            <div className={Styles.rightData}>
              {getOneVendor?.contact_person
                ? `${getOneVendor?.contact_person}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor Contact Number</div>
            <div className={Styles.rightData}>
              {getOneVendor?.contact_phone_no
                ? `${getOneVendor?.contact_phone_no}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor Contact Email</div>
            <div className={Styles.rightData}>
              {getOneVendor?.contact_email
                ? `${getOneVendor?.contact_email}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor Address</div>
            <div className={Styles.rightData}>
              {getOneVendor?.address ? (
                <>
                  {getOneVendor?.address.street && (
                    <>{getOneVendor?.address.street}, </>
                  )}
                  {getOneVendor?.address.city && (
                    <>{getOneVendor?.address.city}, </>
                  )}
                  {getOneVendor?.address.state && (
                    <>{getOneVendor?.address.state}, </>
                  )}
                  {getOneVendor?.address.country && (
                    <>{getOneVendor?.address.country}, </>
                  )}
                  {getOneVendor?.address.pin_code}
                </>
              ) : (
                'Address not available'
              )}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor Bank Account Number</div>
            <div className={Styles.rightData}>
              {getOneVendor?.bank_account_details?.account_no
                ? `${getOneVendor?.bank_account_details?.account_no}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor IFSC Code</div>
            <div className={Styles.rightData}>
              {getOneVendor?.bank_account_details?.ifsc_code
                ? `${getOneVendor?.bank_account_details?.ifsc_code}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Bank Account Holder Name</div>
            <div className={Styles.rightData}>
              {getOneVendor?.bank_account_details?.acc_holder_name
                ? `${getOneVendor?.bank_account_details?.acc_holder_name}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Bank Name</div>
            <div className={Styles.rightData}>
              {getOneVendor?.bank_account_details?.bank_name
                ? `${getOneVendor?.bank_account_details?.bank_name}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Preffered Payment Type</div>
            <div className={Styles.rightData}>
              {getOneVendor?.preferred_payment_method_data?.master_data_name
                ? `${getOneVendor?.preferred_payment_method_data?.master_data_name}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Currency</div>
            <div className={Styles.rightData}>
              {getOneVendor?.currency
                ? `${getOneVendor?.currency}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Tax Number</div>
            <div className={Styles.rightData}>
              {getOneVendor?.tax_id
                ? `${getOneVendor?.tax_id}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Payment Terms</div>
            <div className={Styles.rightData}>
              {getOneVendor?.payment_terms
                ? `${getOneVendor?.payment_terms}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Vendor Category Type</div>
            <div className={Styles.rightData}>
              {getOneVendor?.vendor_category_data?.master_data_name
                ? `${getOneVendor?.vendor_category_data?.master_data_name}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Lead Time</div>
            <div className={Styles.rightData}>
              {getOneVendor?.lead_time
                ? `${getOneVendor?.lead_time}`
                : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Minimum Order Quantity</div>
            <div className={Styles.rightData}>
              {getOneVendor?.minimum_order_quantity
                ? `${getOneVendor?.minimum_order_quantity}`
                : 'Not Provided'}
            </div>
          </div>

          <div className={Styles.dividerStyle}></div>
          <div className={Styles.dataRows}>
            <div className={Styles.leftData}>Notes</div>
            <div className={Styles.rightData}>
              {getOneVendor?.notes ? `${getOneVendor?.notes}` : 'Not Provided'}
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
        </CustomCard>
      </div>
    </div>
  );
};

export default VendorView;
