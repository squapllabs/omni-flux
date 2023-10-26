import catchAsync from '../utils/catchAsync';
import * as vendorQuotationDetailsService from '../services/vendorQuotationDetails.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const updateVendorQuotationDetails = catchAsync(async (req, res) => {
  const methodName = '/updateVendorQuotationDetails';
  try {
    const vendorQuotationDetails =
      await vendorQuotationDetailsService.updateVendorQuotationDetails(
        req.body
      );
    res.send(vendorQuotationDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByVendorQuotesId = catchAsync(async (req, res) => {
  const methodName = '/getByVendorQuotesId';
  try {
    const vendorQuotationDetails =
      await vendorQuotationDetailsService.getByVendorQuotesId(
        req.params.vendor_quotes_id
      );
    res.send(vendorQuotationDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { updateVendorQuotationDetails, getByVendorQuotesId };
