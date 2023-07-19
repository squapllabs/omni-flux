import productDao from '../dao/product.dao';
import {createProductBody,updateProductBody} from '../interfaces/product.interface';
import prisma from '../utils/prisma';

/**
 * Method to Create a new product
 * @param body
 * @returns
 */
const createProduct = async (body: createProductBody) => {
    let result = null;
    try {
      const {
        product_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
      } = body;
  
      result = await prisma
        .$transaction(async (prisma) => {
          const CreateProduct = await productDao.add(
            product_name,
            sub_sub_category_id,
            description,
            hsn_code_id,
            gst_id,
            uom_id,
            created_by,
            updated_by,
            prisma
          );

          return CreateProduct;
        })
        .then((data) => {
          console.log('Successfully Product Data Returned ', data);
          const newProductData = {
            success: true,
            data: data,
          };
          return newProductData;
        })
        .catch((error: string) => {
          console.log('Failure, ROLLBACK was executed', error);
          throw error;
        });
      return result;
    } catch (error) {
      console.log('Error occurred in product service Add: ', error);
      throw error;
    }
  };
/**
 * Method to Add a bulk product
 * @param body
 * @returns
 */

const createProductBulk = async (products: createProductBody[]) => {
  try {
    const newProducts: any[] = [];
    for (const product of products) {
      const {
        product_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
      } = product;

      newProducts.push({
        product_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
        created_date: new Date(),
        updated_date: new Date(),
      });
    }

    const result = await productDao.addBulk(newProducts);

    console.log('Successfully inserted bulk products', result);

    return {
      success: true,
      data: result,
    };
  } catch (error) {
    console.log('Error occurred in user service Add: ', error);
    throw error;
  }
};






  /**
 * Method to Get All product
 * @returns
 */
const getAllProduct = async () => {
  try {
    const result = await productDao.getAll();
    const productData = { success: true, data: result };
    return productData;
  } catch (error) {
    console.log('Error occurred in getAll Product  service : ', error);
    throw error;
  }
};
/**
 * Method to get product By productId
 * @param productId
 * @returns
 */
const getById = async (productId: number) => {
  try {
    let result = null;
    const productData = await productDao.getById(productId);
    if (productData) {
      result = { success: true, data: productData };
      return result;
    } else {
      result = { success: false, message: 'product id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById product service : ', error);
    throw error;
  }
};
/**
 * Method to delete product
 * @param productId
 */
const deleteProduct = async (productId: number) => {
  try {
    const productExist = await productDao.getById(productId);
    if (!productExist) {
      const result = { success: false, message: 'product Id Not Exist' };
      return result;
    }
    const data = await productDao.deleteProduct(productId);
    if (data) {
      const result = {
        success: true,
        message: 'product Data Deleted Successfully',
      };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this product' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteProduct product service : ', error);
    throw error;
  }
};
/**
 * Method to Update an Existing product
 * @param body
 * @returns
 */
const updateProduct = async (body: updateProductBody) => {
  try {
    const { product_id, product_name,sub_sub_category_id , description,  hsn_code_id,gst_id,uom_id,updated_by} = body;
    let result = null;
    const productExist = await productDao.getById(product_id);
    if (productExist) {
      const productDetails = await productDao.edit(
          product_id,
          product_name,
          sub_sub_category_id,
          description,
          hsn_code_id,
          gst_id,
          uom_id,
          updated_by,
      );
      result = { success: true, data: productDetails };
      return result;
    } else {
      result = { success: false, message: 'product_id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in product service Edit: ', error);
    throw error;
  }
};
export {
    createProduct,
    getAllProduct,
    getById,
    deleteProduct,
    updateProduct,
    createProductBulk
}