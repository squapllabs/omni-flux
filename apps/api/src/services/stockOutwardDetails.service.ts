import stockOutwardDao from '../dao/stockOutward.dao';
import stockOutwardDetailsDao from '../dao/stockOutwardDetails.dao';

const getByStockOutwardId = async (stockOutwardId: number) => {
  try {
    const stockOutwardExist = await stockOutwardDao.getById(stockOutwardId);
    if (!stockOutwardExist) {
      return {
        message: 'stock_outward_id does not exist',
        status: false,
        data: null,
      };
    }
    const stockOutwardDetails =
      await stockOutwardDetailsDao.getByStockOutwardId(stockOutwardId);
    if (stockOutwardDetails.length > 0) {
      return {
        message: 'success',
        status: true,
        data: stockOutwardDetails,
      };
    } else {
      return {
        message: 'No data found for this stock_outward_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error Occurred in Stock Outward Details Service : getByStockOutwardId ',
      error
    );
    throw error;
  }
};

export { getByStockOutwardId };
