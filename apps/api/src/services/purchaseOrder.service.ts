import purchaseOrderDao from '../dao/purchaseOrder.dao';
import purchaseRequestDao from '../dao/purchaseRequest.dao';
import vendorDao from '../dao/vendor.dao';
import { purchaseOrderBody } from '../interfaces/purchaseOrder.interface';

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
      created_by
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

    const purchaseOrderDetails = await purchaseOrderDao.edit(
      purchase_request_id,
      vendor_id,
      order_date,
      status,
      total_cost,
      order_remark,
      updated_by,
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

    const filterObj: any = {};

    if (status) {
      filterObj.filterPurchaseOrder = {
        is_delete: status === 'AC' ? false : true,
      };
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

export {
  createPurchaseOrder,
  updatePurchaseOrder,
  getAllPurchaseOrders,
  getById,
  deletePurchaseOrder,
  searchPurchaseOrder,
};
