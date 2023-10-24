import prisma from '../utils/prisma';

const searchIndentRequestDetails = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterIndentRequestDetails;
    const indentRequestDetails =
      await transaction.indent_request_details.findMany({
        where: filter,
        include: {
          bom_detail_data: {
            include: {
              uom_data: { select: { name: true } },
              sub_category_data: { select: { name: true } },
              item_data: true,
              labour_data: true,
              machinery_data: true,
            },
          },
          indent_request_data: { include: { project_data: true } },
        },
        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
    const indentRequestDetailsCount =
      await transaction.indent_request_details.count({
        where: filter,
      });
    const indentRequestDetailsData = {
      count: indentRequestDetailsCount,
      data: indentRequestDetails,
    };
    return indentRequestDetailsData;
  } catch (error) {
    console.log(
      'Error occurred in indentRequestDetails dao : searchIndentRequestDetails',
      error
    );
    throw error;
  }
};

const updatePurchaseRequestQuantity = async (
  indentRequestDetailsId: number,
  purchaseRequestedQuantity: number,
  purchaseRemainingQuantity: number,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const currentDate = new Date();
    const indentRequestDetails =
      await transaction.indent_request_details.update({
        where: {
          indent_request_details_id: Number(indentRequestDetailsId),
        },
        data: {
          purchase_requested_quantity: purchaseRequestedQuantity,
          purchase_remaining_quantity: purchaseRemainingQuantity,
          updated_date: currentDate,
          updated_by,
        },
      });
    return indentRequestDetails;
  } catch (error) {
    console.log(
      'Error occurred in indentRequestDetails updatePurchaseRequestQuantity dao',
      error
    );
    throw error;
  }
};

export default { searchIndentRequestDetails, updatePurchaseRequestQuantity };
