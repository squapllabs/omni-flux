import prisma from '../utils/prisma';

const getById = async (
  leadEnquiryProductItemId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadEnquiryProductItem =
      await transaction.lead_enquiry_product_item.findUnique({
        where: {
          lead_enquiry_product_item_id: Number(leadEnquiryProductItemId),
        },
      });
    return leadEnquiryProductItem;
  } catch (error) {
    console.log('Error occurred in leadEnquiryProductItem getById dao', error);
    throw error;
  }
};

export default {
  getById,
};
