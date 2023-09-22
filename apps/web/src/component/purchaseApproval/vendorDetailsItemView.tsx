import React from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import Styles from '../../styles/vendorDetailItemView.module.scss';
import BackArrowIcon from '../menu/icons/backArrow';
import { getByQuoteVendorId } from '../../hooks/vendorQuotes-hooks';
import CustomLoader from '../ui/customLoader';

const VendorDetailItemView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const VendorId = Number(routeParams?.id);
  console.log('vendor id', VendorId);
  const { data: getOneVendor, isLoading: dataLoading } =
    getByQuoteVendorId(VendorId);
  console.log('getOneVendor==>', getOneVendor);

  return (
    <div>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.title}>
          <div>
            <h3>
              Item Details requested to {getOneVendor?.vendor_data?.vendor_name}{' '}
              Vendor
            </h3>
          </div>
          <div>
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              color="primary"
              icon={<BackArrowIcon />}
              onClick={() =>
                navigate(`/vendor-select/${getOneVendor?.purchase_request_id}`)
              }
            >
              Back
            </Button>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Item Name</th>
                  <th>Quantity</th>
                </tr>
              </thead>
              <tbody>
                {getOneVendor?.quotation_details?.map(
                  (data: any, index: number) => {
                    return (
                      <tr key={data.indent_request_id}>
                        <td>{index + 1}</td>
                        <td>{data?.item_name}</td>
                        <td>{data?.quantity}</td>
                      </tr>
                    );
                  }
                )}
              </tbody>
            </table>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default VendorDetailItemView;
