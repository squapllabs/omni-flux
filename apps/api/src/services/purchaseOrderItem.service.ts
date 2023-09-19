import itemDao from '../dao/item.dao';
import purchaseOrderDao from '../dao/purchaseOrder.dao';
import purchaseOrderItemDao from '../dao/purchaseOrderItem.dao';
import { purchaseOrderItemBody } from '../interfaces/purchaseOrderItem.interface';
import s3 from '../utils/s3';
import fs from 'fs';

/**
 * Method to Create a New PurchaseOrderItem
 * @param body
 * @returns
 */
const createPurchaseOrderItem = async (body: purchaseOrderItemBody) => {
  try {
    const {
      purchase_order_id,
      item_id,
      order_quantity,
      unit_price,
      created_by,
      purchase_order_item_documents,
    } = body;

    if (purchase_order_id) {
      const purchaseOrderExist = await purchaseOrderDao.getById(
        purchase_order_id
      );
      if (!purchaseOrderExist) {
        return {
          message: 'purchase_order_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (item_id) {
      const itemExist = await itemDao.getById(item_id);
      if (!itemExist) {
        return {
          message: 'item_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const purchaseOrderItemDetails = await purchaseOrderItemDao.add(
      purchase_order_id,
      item_id,
      order_quantity,
      unit_price,
      created_by,
      purchase_order_item_documents
    );
    const result = {
      message: 'success',
      status: true,
      data: purchaseOrderItemDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in purchaseOrderItem service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing PurchaseOrderItem
 * @param body
 * @returns
 */

const updatePurchaseOrderItem = async (req) => {
  try {
    const body = req.body;
    const files = req?.files?.purchase_order_item_documents;
    const {
      purchase_order_id,
      item_id,
      order_quantity,
      unit_price,
      updated_by,
      purchase_order_item_id,
    } = body;
    let result = null;
    const purchaseOrderItemExist = await purchaseOrderItemDao.getById(
      purchase_order_item_id
    );
    if (!purchaseOrderItemExist) {
      result = {
        message: 'purchase_order_item_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (purchase_order_id) {
      const purchaseOrderExist = await purchaseOrderDao.getById(
        purchase_order_id
      );
      if (!purchaseOrderExist) {
        return {
          message: 'purchase_order_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (item_id) {
      const itemExist = await itemDao.getById(item_id);
      if (!itemExist) {
        return {
          message: 'item_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    /* Purchase Order Item Document File Handling */

    const purchaseOrderItemDocuments = [];
    let index = 0;
    if (files) {
      const existingDocument =
        purchaseOrderItemExist?.purchase_order_item_documents;

      if (existingDocument?.length > 0) {
        for (const doc of existingDocument) {
          const existingDocPath = doc.path;
          await s3.deleteFileFromS3UsingPath(existingDocPath);
          console.log('Existing File deleted successfully.');
        }
      }

      for (const file of files) {
        const code = `purchase-order-item-${purchase_order_item_id}-${index}`;
        const localUploadedFilePath = file.path;
        const fileData = await s3.uploadFileInS3(
          file,
          code,
          'purchase-order-item'
        );

        const s3FilePath = fileData.path;

        fs.unlink(localUploadedFilePath, (err) => {
          if (err) {
            console.error('Error deleting the local file:', err);
          } else {
            console.log('Local File deleted successfully.');
          }
        });

        purchaseOrderItemDocuments.push({
          index,
          path: s3FilePath,
          folder: 'purchase-order-item',
          code: code,
        });
        index++;
      }
    }

    const purchaseOrderItemDetails = await purchaseOrderItemDao.edit(
      Number(purchase_order_id),
      Number(item_id),
      Number(order_quantity),
      Number(unit_price),
      Number(updated_by),
      purchaseOrderItemDocuments,
      Number(purchase_order_item_id)
    );
    result = {
      message: 'success',
      status: true,
      data: purchaseOrderItemDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in purchaseOrderItem service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get PurchaseOrderItem By PurchaseOrderItemId
 * @param purchaseOrderItemId
 * @returns
 */
const getById = async (purchaseOrderItemId: number) => {
  try {
    let result = null;
    const purchaseOrderItemData = await purchaseOrderItemDao.getById(
      purchaseOrderItemId
    );
    if (purchaseOrderItemData) {
      result = {
        message: 'success',
        status: true,
        data: purchaseOrderItemData,
      };
      return result;
    } else {
      result = {
        message: 'purchase_order_item_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getById purchaseOrderItem service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All PurchaseOrderItems
 * @returns
 */
const getAllPurchaseOrderItems = async () => {
  try {
    const result = await purchaseOrderItemDao.getAll();
    const purchaseOrderItemData = {
      message: 'success',
      status: true,
      data: result,
    };
    return purchaseOrderItemData;
  } catch (error) {
    console.log(
      'Error occurred in getAllPurchaseOrderItems purchaseOrderItem service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete purchaseOrderItem
 * @param purchaseOrderItemId
 */
const deletePurchaseOrderItem = async (purchaseOrderItemId: number) => {
  try {
    const purchaseOrderItemExist = await purchaseOrderItemDao.getById(
      purchaseOrderItemId
    );

    if (!purchaseOrderItemExist) {
      const result = {
        message: 'purchase_order_item_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await purchaseOrderItemDao.deletePurchaseOrderItem(
      purchaseOrderItemId
    );
    if (data) {
      const result = {
        message: 'PurchaseOrderItem Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this purchaseOrderItem',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deletePurchaseOrderItem purchaseOrderItem service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search PurchaseOrderItem - Pagination API
 * @returns
 */
const searchPurchaseOrderItem = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterPurchaseOrderItem = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (global_search) {
      filterObj.filterPurchaseOrderItem =
        filterObj.filterPurchaseOrderItem || {};
      filterObj.filterPurchaseOrderItem.OR =
        filterObj.filterPurchaseOrderItem.OR || [];

      filterObj.filterPurchaseOrderItem.OR.push(
        {
          purchase_order_data: {
            status: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          purchase_order_data: {
            order_remark: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          item_data: {
            item_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await purchaseOrderItemDao.searchPurchaseOrderItem(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempPurchaseOrderItemData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempPurchaseOrderItemData;
  } catch (error) {
    console.log(
      'Error occurred in searchPurchaseOrderItem PurchaseOrderItem service : ',
      error
    );
    throw error;
  }
};

export {
  createPurchaseOrderItem,
  updatePurchaseOrderItem,
  getAllPurchaseOrderItems,
  getById,
  deletePurchaseOrderItem,
  searchPurchaseOrderItem,
};
