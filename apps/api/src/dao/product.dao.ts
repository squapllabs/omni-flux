import prisma from '../utils/prisma';
const add = async (
  product_name: string,
  sub_sub_category_id: number,
  description: string,
  hsn_code_id:number,
  gst_id:number,
  uom_id:number,
  created_by: bigint,
  updated_by: bigint,
  connectionObj = null
  ) => {
    try {
      const currentDate = new Date();
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const product = await transaction.product.create({
        data: {
          product_name,
          sub_sub_category_id,
          description,
          hsn_code_id,
          gst_id,
          uom_id,
          created_by,
          updated_by,
          created_date: currentDate,
          updated_date: currentDate,
        },
      });
      return product;
    } catch (error) {
      console.log('Error occurred in productDao add dao', error);
      throw error;
    }
  };
// product.dao.ts

const addBulk = async (products: any[], connectionObj = null) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    
    const createdProducts = await transaction.product.createMany({
      data: products.map((product) => ({
        ...product,
        created_date: currentDate,
        updated_date: currentDate,
      })),
    });

    return createdProducts;
  } catch (error) {
    console.log('Error occurred in productDao addBulk dao', error);
    throw error;
  }
};

  
const getAll = async (connectionObj = null) => {
    try {
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const product = await transaction.product.findMany({
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
      return product;
    } catch (error) {
      console.log('Error occurred in product getAll dao', error);
      throw error;
    }
  };
  const getById = async (productId: number, connectionObj = null) => {
    try {
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const product = await transaction.product.findUnique({
        where: {
          product_id: Number(productId),
        },
      });
      return product;
    } catch (error) {
      console.log('Error occurred in product getById dao', error);
      throw error;
    }
  };
  const deleteProduct = async (productId: number, connectionObj = null) => {
    try {
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const product = await transaction.product.delete({
        where: {
          product_id: Number(productId),
        },
      });
      return product;
    } catch (error) {
      console.log('Error occurred in product delete dao', error);
      throw error;
    }
  };
  const edit = async (
  product_id:number,  
  product_name: string,
  sub_sub_category_id: number,
  description: string,
  hsn_code_id:number,
  gst_id:number,
  uom_id:number,
  updated_by: bigint,
  connectionObj = null
  ) => {
    try {
      const currentDate = new Date();
      const transaction = connectionObj !== null ? connectionObj : prisma;
      const product = await transaction.product.update({
        where: {
          product_id: product_id,
        },
        data: {  
          product_name,
          sub_sub_category_id,
          description,
          hsn_code_id,
          gst_id,
          uom_id,
          updated_by,
          updated_date: currentDate,
        },
      });
      return product;
    } catch (error) {
      console.log('Error occurred in productDao edit', error);
      throw error;
    }
  };
  export default{
    add,
    getAll,
    getById,
    deleteProduct,
    edit,
    addBulk
  }