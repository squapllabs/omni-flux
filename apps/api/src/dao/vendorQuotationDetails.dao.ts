import prisma from '../utils/prisma';

const edit = async (
  vendor_quotes_id: number,
  item_id: number,
  indent_request_details_id: number,
  indent_requested_quantity: number,
  purchase_requested_quantity: number,
  unit_cost: number,
  total_cost: number,
  updated_by: number,
  vendor_quotation_details_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotationDetails =
      await transaction.vendor_quotation_details.update({
        where: {
          vendor_quotation_details_id: vendor_quotation_details_id,
        },
        data: {
          vendor_quotes_id,
          item_id,
          indent_request_details_id,
          indent_requested_quantity,
          purchase_requested_quantity,
          unit_cost,
          total_cost,
          updated_by,
          updated_date: currentDate,
        },
      });
    return vendorQuotationDetails;
  } catch (error) {
    console.log('Error occurred in vendorQuotationDetailsDao edit', error);
    throw error;
  }
};

const getById = async (
  vendor_quotation_details_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotationDetails =
      await transaction.vendor_quotation_details.findFirst({
        where: {
          vendor_quotation_details_id: Number(vendor_quotation_details_id),
          is_delete: false,
        },
      });
    return vendorQuotationDetails;
  } catch (error) {
    console.log('Error occurred in vendorQuotationDetails getById dao', error);
    throw error;
  }
};

const getByVendorQuotesId = async (
  vendor_quotes_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotationDetails =
      await transaction.vendor_quotation_details.findMany({
        where: {
          vendor_quotes_id: Number(vendor_quotes_id),
          is_delete: false,
        },
      });
    return vendorQuotationDetails;
  } catch (error) {
    console.log(
      'Error occurred in vendorQuotationDetails getByVendorQuotesId dao',
      error
    );
    throw error;
  }
};

export default {
  edit,
  getById,
  getByVendorQuotesId,
};
