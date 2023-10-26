import vendorQuotationDetailsDao from '../dao/vendorQuotationDetails.dao';
import { vendorQuotationDetailsBody } from '../interfaces/vendorQuotationDetails.interface';
/**
 * Method to Update an Existing VendorQuotationDetails
 * @param body
 * @returns
 */

const updateVendorQuotationDetails = async (
  body: vendorQuotationDetailsBody
) => {
  try {
    const {
      vendor_quotes_id,
      item_id,
      indent_request_details_id,
      indent_requested_quantity,
      purchase_requested_quantity,
      unit_cost,
      total_cost,
      updated_by,
      vendor_quotation_details_id,
    } = body;

    const vendorQuotationDetailsExist = await vendorQuotationDetailsDao.getById(
      vendor_quotation_details_id
    );

    if (vendorQuotationDetailsExist) {
      const vendorQuotationDetailsData = await vendorQuotationDetailsDao.edit(
        vendor_quotes_id,
        item_id,
        indent_request_details_id,
        indent_requested_quantity,
        purchase_requested_quantity,
        unit_cost,
        total_cost,
        updated_by,
        vendor_quotation_details_id
      );
      return {
        message: 'success',
        status: true,
        data: vendorQuotationDetailsData,
      };
    } else {
      return {
        message: 'vendor_quotation_details_id does not exist',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in vendorQuotationDetails service Edit: ',
      error
    );
    throw error;
  }
};

export { updateVendorQuotationDetails };
