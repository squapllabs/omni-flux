import vendorQuotationDetailsDao from '../dao/vendorQuotationDetails.dao';
import vendorQuotesDao from '../dao/vendorQuotes.dao';
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

/**
 * Method to get Vendor Quotation Details By Vendor Quoted Id
 * @param vendorQuotesId
 * @returns
 */
const getByVendorQuotesId = async (vendorQuotesId: number) => {
  try {
    const vendorQuotesExist = await vendorQuotesDao.getById(vendorQuotesId);
    if (!vendorQuotesExist) {
      return {
        message: 'vendor_quotes_id does not exist',
        status: false,
        data: null,
      };
    }
    const vendorQuotationDetailsData =
      await vendorQuotationDetailsDao.getByVendorQuotesId(vendorQuotesId);
    if (vendorQuotationDetailsData.length > 0) {
      return {
        message: 'success',
        status: true,
        data: vendorQuotationDetailsData,
      };
    } else {
      return {
        message: 'No data found for this vendor_quotes_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in getByVendorQuotesId vendorQuotationDetails service : ',
      error
    );
    throw error;
  }
};
export { updateVendorQuotationDetails, getByVendorQuotesId };
