import catchAsync from '../utils/catchAsync';
import * as stockOutwardDetailsService from '../services/stockOutwardDetails.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const getByStockOutwardId = catchAsync(async (req, res) => {
  const methodName = '/getByStockOutwardId';
  try {
    const stockOutward = await stockOutwardDetailsService.getByStockOutwardId(
      req.params.stock_outward_id
    );
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { getByStockOutwardId };
