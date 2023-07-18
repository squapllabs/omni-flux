import prisma from '../utils/prisma';
const add = async (
    product_id: number,
    transaction_type: string,
    quantity: number,
    transaction_date:string,
    warehouse_id:number,
    site_id:number,
    created_by:bigint,
    updated_by:bigint,
    connectionObj = null
  ) => {
    try {
      const currentDate = new Date();
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const stock = await transaction.stock.create({
        data: {
          product_id,
          transaction_type,
          quantity,
          transaction_date,
          warehouse_id,
          site_id,
          created_by,
          updated_by,
          created_date: currentDate,
          updated_date: currentDate,
        },
      });
      return stock;
    } catch (error) {
      console.log('Error occurred in stockDao add dao', error);
      throw error;
    }
  };
  
const getAll = async (connectionObj = null) => {
    try {
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const stock = await transaction.stock.findMany({
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
      return stock;
    } catch (error) {
      console.log('Error occurred in stock getAll dao', error);
      throw error;
    }
  };
const getById = async (stocktId: number, connectionObj = null) => {
    try {
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const stock = await transaction.stock.findUnique({
        where: {
          product_id: Number(stocktId),
        },
      });
      return stock;
    } catch (error) {
      console.log('Error occurred in stock getById dao', error);
      throw error;
    }
  };
  const deleteStock = async (stockId: number, connectionObj = null) => {
    try {
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const stock = await transaction.stock.delete({
        where: {
          product_id: Number(stockId),
        },
      });
      return stock;
    } catch (error) {
      console.log('Error occurred in stock delete dao', error);
      throw error;
    }
  };
  const edit = async (
    stock_id:number,
    product_id: number,
    transaction_type: string,
    quantity: number,
    transaction_date:string,
    warehouse_id:number,
    site_id:number,
    updated_by:bigint,
    connectionObj = null
  ) => {
    try {
      const currentDate = new Date();
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const product = await transaction.stock.update({
        where: {
          stock_id: stock_id,
        },
        data: {  
            product_id,
            transaction_type,
            quantity,
            transaction_date,
            warehouse_id,
            site_id,
            updated_by,
            updated_date: currentDate,
        },
      });
      return product;
    } catch (error) {
      console.log('Error occurred in stockDao edit', error);
      throw error;
    }
  };
  export default{
    add,
    getAll,
    getById,
    deleteStock,
    edit,
  }