import stockDao from '../dao/stock.dao';
import {createStockBody,updateStockBody} from '../interfaces/stock.interface';
import prisma from '../utils/prisma';

/**
 * Method to Create a new stock
 * @param body
 * @returns
 */
const createStock = async (body: createStockBody) => {
    let result = null;
    try {
      const {
        product_id,
        transaction_type,
        quantity,
        transaction_date,
        warehouse_id,
        site_id,
        created_by,
        updated_by,
      } = body;
  
      result = await prisma
        .$transaction(async (prisma) => {
          const CreateStock = await stockDao.add(
        product_id,
        transaction_type,
        quantity,
        transaction_date,
        warehouse_id,
        site_id,
        created_by,
        updated_by,
            prisma
          );

          return CreateStock;
        })
        .then((data) => {
          console.log('Successfully stock Data Returned ', data);
          const newStockData = {
            success: true,
            data: data,
          };
          return newStockData;
        })
        .catch((error: string) => {
          console.log('Failure, ROLLBACK was executed', error);
          throw error;
        });
      return result;
    } catch (error) {
      console.log('Error occurred in stock service Add: ', error);
      throw error;
    }
  };

/* * Method to Get All stock
 * @returns
 */
const getAllStock = async () => {
  try {
    const result = await stockDao.getAll();
    const stockData = { success: true, data: result };
    return stockData;
  } catch (error) {
    console.log('Error occurred in getAll stock service : ', error);
    throw error;
  }
};
/**
 * Method to get product By stockId
 * @param StockId
 * @returns
 */
const getById = async (StockId: number) => {
  try {
    let result = null;
    const stockData = await stockDao.getById(StockId);
    if (stockData) {
      result = { success: true, data: stockData };
      return result;
    } else {
      result = { success: false, message: 'stock id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById stock service : ', error);
    throw error;
  }
};
/**
 * Method to delete stock
 * @param stockId
 */
const deleteStock = async (stockId: number) => {
  try {
    const productExist = await stockDao.getById(stockId);
    if (!productExist) {
      const result = { success: false, message: 'product Id Not Exist' };
      return result;
    }
    const data = await stockDao.deleteStock(stockId);
    if (data) {
      const result = {
        success: true,
        message: 'stock Data Deleted Successfully',
      };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this stock' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteProduct stock service : ', error);
    throw error;
  }
};
/**
 * Method to Update an Existing stock
 * @param body
 * @returns
 */
const updateStock = async (body: updateStockBody) => {
  try {
    const {
        stock_id,
        product_id,
        transaction_type,
        quantity,
        transaction_date,
        warehouse_id,
        site_id,
        updated_by,
    } = body;
    let result = null;
    const stockExist = await stockDao.getById(stock_id);
    if (stockExist) {
      const stockDetails = await stockDao.edit(
        stock_id,
        product_id,
        transaction_type,
        quantity,
        transaction_date,
        warehouse_id,
        site_id,
        updated_by,
      );
      result = { success: true, data: stockDetails };
      return result;
    } else {
      result = { success: false, message: 'stock_id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in stock service Edit: ', error);
    throw error;
  }
};
export {
    createStock,
    updateStock,
    deleteStock,
    getAllStock,
    getById,
}