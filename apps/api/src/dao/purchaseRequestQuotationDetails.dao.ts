import prisma from '../utils/prisma';

const add = async (
  purchase_request_id: number,
  item_id: number,
  indent_request_details_id: number,
  indent_requested_quantity: number,
  purchase_requested_quantity: number,
  unit_cost: number,
  total_cost: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequestQuotationDetails =
      await transaction.purchase_request_quotation_details.create({
        data: {
          purchase_request_id,
          item_id,
          indent_request_details_id,
          indent_requested_quantity,
          purchase_requested_quantity,
          unit_cost,
          total_cost,
          created_by,
          created_date: currentDate,
          updated_date: currentDate,
          is_delete: is_delete,
        },
      });
    return purchaseRequestQuotationDetails;
  } catch (error) {
    console.log(
      'Error occurred in purchaseRequestQuotationDetailsDao add',
      error
    );
    throw error;
  }
};

export default {
  add,
};
