import prisma from '../utils/prisma';

const getByStockOutwardId = async (
  stockOutwardId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockOutwardDetails =
      await transaction.stock_outward_details.findMany({
        where: {
          stock_outward_id: Number(stockOutwardId),
          is_delete: false,
        },
        include: {
          item_data: true,
          uom_data: { select: { name: true } },
        },
        orderBy: [{ updated_date: 'desc' }],
      });
    return stockOutwardDetails;
  } catch (error) {
    console.log('Error occurred in stockOutwardDetails getById dao', error);
    throw error;
  }
};

export default {
  getByStockOutwardId,
};
