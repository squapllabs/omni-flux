import itemDao from '../dao/item.dao';
import projectInventoryDao from '../dao/projectInventory.dao';
import purchaseOrderDao from '../dao/purchaseOrder.dao';
import purchaseOrderItemDao from '../dao/purchaseOrderItem.dao';
import purchaseRequestDao from '../dao/purchaseRequest.dao';
import vendorDao from '../dao/vendor.dao';
import { purchaseOrderBody } from '../interfaces/purchaseOrder.interface';
import { processFileDeleteInS3 } from '../utils/fileUpload';
import prisma from '../utils/prisma';

/**
 * Method to Create a New PurchaseOrder
 * @param body
 * @returns
 */
const createPurchaseOrder = async (body: purchaseOrderBody) => {
  try {
    const {
      purchase_request_id,
      vendor_id,
      order_date,
      status,
      total_cost,
      order_remark,
      created_by,
      purchase_order_documents,
      payment_mode,
      payment_date,
    } = body;

    if (purchase_request_id) {
      const purchaseRequestExist = await purchaseRequestDao.getById(
        purchase_request_id
      );
      if (!purchaseRequestExist) {
        return {
          message: 'purchase_request_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (vendor_id) {
      const vendorExist = await vendorDao.getById(vendor_id);
      if (!vendorExist) {
        return {
          message: 'vendor_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const purchaseOrderDetails = await purchaseOrderDao.add(
      purchase_request_id,
      vendor_id,
      order_date,
      status,
      total_cost,
      order_remark,
      created_by,
      purchase_order_documents,
      payment_mode,
      payment_date
    );
    const result = {
      message: 'success',
      status: true,
      data: purchaseOrderDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in purchaseOrder service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing PurchaseOrder
 * @param body
 * @returns
 */

const updatePurchaseOrder = async (body: purchaseOrderBody) => {
  try {
    const {
      purchase_request_id,
      vendor_id,
      order_date,
      status,
      total_cost,
      order_remark,
      updated_by,
      purchase_order_documents,
      payment_mode,
      payment_date,
      purchase_order_id,
    } = body;
    let result = null;
    const purchaseOrderExist = await purchaseOrderDao.getById(
      purchase_order_id
    );
    if (!purchaseOrderExist) {
      result = {
        message: 'purchase_order_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (purchase_request_id) {
      const purchaseRequestExist = await purchaseRequestDao.getById(
        purchase_request_id
      );
      if (!purchaseRequestExist) {
        return {
          message: 'purchase_request_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (vendor_id) {
      const vendorExist = await vendorDao.getById(vendor_id);
      if (!vendorExist) {
        return {
          message: 'vendor_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const updatedPurchaseOrderDocuments = [];
    if (purchase_order_documents) {
      for (const doc of purchase_order_documents) {
        const { is_delete, path } = doc;

        if (is_delete === true) {
          const deleteDocInS3Body = {
            path,
          };
          await processFileDeleteInS3(deleteDocInS3Body);
        } else {
          updatedPurchaseOrderDocuments.push(doc);
        }
      }
    }

    const purchaseOrderDetails = await purchaseOrderDao.edit(
      purchase_request_id,
      vendor_id,
      order_date,
      status,
      total_cost,
      order_remark,
      updated_by,
      updatedPurchaseOrderDocuments,
      payment_mode,
      payment_date ? payment_date : purchaseOrderExist?.payment_date,
      purchase_order_id
    );
    result = { message: 'success', status: true, data: purchaseOrderDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in purchaseOrder service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get PurchaseOrder By PurchaseOrderId
 * @param purchaseOrderId
 * @returns
 */
const getById = async (purchaseOrderId: number) => {
  try {
    let result = null;
    const purchaseOrderData = await purchaseOrderDao.getById(purchaseOrderId);
    if (purchaseOrderData) {
      result = { message: 'success', status: true, data: purchaseOrderData };
      return result;
    } else {
      result = {
        message: 'purchase_order_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById purchaseOrder service : ', error);
    throw error;
  }
};

/**
 * Method to Get All PurchaseOrders
 * @returns
 */
const getAllPurchaseOrders = async () => {
  try {
    const result = await purchaseOrderDao.getAll();
    const purchaseOrderData = {
      message: 'success',
      status: true,
      data: result,
    };
    return purchaseOrderData;
  } catch (error) {
    console.log(
      'Error occurred in getAllPurchaseOrders purchaseOrder service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete purchaseOrder
 * @param purchaseOrderId
 */
const deletePurchaseOrder = async (purchaseOrderId: number) => {
  try {
    const purchaseOrderExist = await purchaseOrderDao.getById(purchaseOrderId);

    if (!purchaseOrderExist) {
      const result = {
        message: 'purchase_order_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await purchaseOrderDao.deletePurchaseOrder(purchaseOrderId);
    if (data) {
      const result = {
        message: 'PurchaseOrder Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this purchaseOrder',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deletePurchaseOrder purchaseOrder service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search PurchaseOrder - Pagination API
 * @returns
 */
const searchPurchaseOrder = async (body) => {
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
    const project_id = body.project_id;
    const bill_status = body.bill_status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterPurchaseOrder = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (project_id) {
      filterObj.filterPurchaseOrder = filterObj.filterPurchaseOrder || {};
      filterObj.filterPurchaseOrder.AND =
        filterObj.filterPurchaseOrder.AND || [];

      filterObj.filterPurchaseOrder.AND.push({
        purchase_request_data: {
          project_id: project_id,
        },
      });
    }

    if (bill_status) {
      filterObj.filterPurchaseOrder = filterObj.filterPurchaseOrder || {};
      filterObj.filterPurchaseOrder.AND =
        filterObj.filterPurchaseOrder.AND || [];

      filterObj.filterPurchaseOrder.AND.push({
        status: bill_status,
      });
    }

    if (global_search) {
      filterObj.filterPurchaseOrder = filterObj.filterPurchaseOrder || {};
      filterObj.filterPurchaseOrder.OR = filterObj.filterPurchaseOrder.OR || [];

      filterObj.filterPurchaseOrder.OR.push(
        {
          status: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          order_remark: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          purchase_request_data: {
            vendor_selection_method: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          vendor_data: {
            vendor_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await purchaseOrderDao.searchPurchaseOrder(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempPurchaseOrderData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempPurchaseOrderData;
  } catch (error) {
    console.log(
      'Error occurred in searchPurchaseOrder PurchaseOrder service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Create a New PurchaseOrder With Purchase Order Item Details
 * @param body
 * @returns
 */
const createPurchaseOrderWithItem = async (body: purchaseOrderBody) => {
  try {
    const {
      purchase_request_id,
      vendor_id,
      order_date,
      status,
      total_cost,
      order_remark,
      created_by,
      purchase_order_item,
    } = body;

    if (purchase_request_id) {
      const purchaseRequestExist = await purchaseRequestDao.getById(
        purchase_request_id
      );
      if (!purchaseRequestExist) {
        return {
          message: 'purchase_request_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (vendor_id) {
      const vendorExist = await vendorDao.getById(vendor_id);
      if (!vendorExist) {
        return {
          message: 'vendor_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const purchaseOrderDetails =
      await purchaseOrderDao.createPurchaseOrderWithItem(
        purchase_request_id,
        vendor_id,
        order_date,
        status,
        total_cost,
        order_remark,
        created_by,
        purchase_order_item
      );
    const result = {
      message: 'success',
      status: true,
      data: purchaseOrderDetails,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrder service createPurchaseOrderWithItem: ',
      error
    );
    throw error;
  }
};

/**
 * Method to get PurchaseOrder By PurchaseRequestId
 * @param purchaseRequestId
 * @returns
 */
const getByPurchaseRequestId = async (purchaseRequestId: number) => {
  try {
    let result = null;
    const purchaseRequestExist = await purchaseRequestDao.getById(
      purchaseRequestId
    );
    if (!purchaseRequestExist) {
      return {
        message: 'purchase_request_id does not exist',
        status: false,
        data: null,
      };
    }

    const purchaseOrderData = await purchaseOrderDao.getByPurchaseRequestId(
      purchaseRequestId
    );
    if (purchaseOrderData) {
      result = { message: 'success', status: true, data: purchaseOrderData };
      return result;
    } else {
      result = {
        message: 'No data found for this purchase_request_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByPurchaseRequestId purchaseOrder service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Update an Existing PurchaseOrder Status And Document
 * @param body
 * @returns
 */

const updateStatusAndDocument = async (body: purchaseOrderBody) => {
  try {
    const {
      status,
      updated_by,
      purchase_order_documents,
      purchase_order_id,
      payment_mode,
      payment_date,
    } = body;
    let result = null;
    const purchaseOrderExist = await purchaseOrderDao.getById(
      purchase_order_id
    );
    if (!purchaseOrderExist) {
      result = {
        message: 'purchase_order_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const project_id = purchaseOrderExist?.purchase_request_data?.project_id;
    const site_id = purchaseOrderExist?.purchase_request_data?.site_id;

    const updatedPurchaseOrderDocuments = [];
    if (purchase_order_documents) {
      for (const doc of purchase_order_documents) {
        const { is_delete, path } = doc;

        if (is_delete === true) {
          const deleteDocInS3Body = {
            path,
          };
          await processFileDeleteInS3(deleteDocInS3Body);
        } else {
          updatedPurchaseOrderDocuments.push(doc);
        }
      }
    }

    const purchaseOrderData = await prisma
      .$transaction(async (tx) => {
        const purchaseOrderDetails =
          await purchaseOrderDao.updateStatusAndDocument(
            status,
            updated_by,
            updatedPurchaseOrderDocuments,
            purchase_order_id,
            payment_mode,
            payment_date ? payment_date : purchaseOrderExist?.payment_date,
            tx
          );

        const projectInventoryDetails = [];

        /* Function to update or create Project Inventory based on status */

        if (status === 'Product Received') {
          const purchaseOrderItemDetails =
            await purchaseOrderItemDao.getByPurchaseOrderId(
              purchase_order_id,
              tx
            );

          for (const purchaseOrderItemDetail of purchaseOrderItemDetails) {
            const item_id = purchaseOrderItemDetail.item_id;
            const order_quantity = purchaseOrderItemDetail.order_quantity;

            const projectItemExistInProjectInventory =
              await projectInventoryDao.getByProjectIdAndItemId(
                project_id,
                item_id,
                tx
              );

            if (projectItemExistInProjectInventory) {
              const rate = projectItemExistInProjectInventory?.rate;

              const updated_available_quantity =
                projectItemExistInProjectInventory?.available_quantity +
                order_quantity;
              const total_cost = rate * updated_available_quantity;

              const updatedProjectInventory =
                await projectInventoryDao.updateQuantityByProjectInventoryId(
                  updated_available_quantity,
                  updated_by,
                  total_cost,
                  projectItemExistInProjectInventory?.project_inventory_id,
                  tx
                );
              projectInventoryDetails.push(updatedProjectInventory);
            } else {
              const itemData = await itemDao.getById(item_id, tx);
              const rate = itemData?.rate;
              const total_cost = rate * order_quantity;

              const newProjectInventory = await projectInventoryDao.add(
                project_id,
                item_id,
                rate,
                order_quantity,
                total_cost,
                updated_by,
                site_id,
                tx
              );
              projectInventoryDetails.push(newProjectInventory);
            }
          }
        }

        const purchaseOrderDetailsData = {
          purchase_order: purchaseOrderDetails,
          project_inventory: projectInventoryDetails,
        };

        result = {
          message: 'success',
          status: true,
          data: purchaseOrderDetailsData,
        };
        return result;
      })
      .then((data) => {
        console.log('Successfully Purchase Order Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return purchaseOrderData;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrder service updateStatusAndDocument: ',
      error
    );
    throw error;
  }
};

export {
  createPurchaseOrder,
  updatePurchaseOrder,
  getAllPurchaseOrders,
  getById,
  deletePurchaseOrder,
  searchPurchaseOrder,
  createPurchaseOrderWithItem,
  getByPurchaseRequestId,
  updateStatusAndDocument,
};
