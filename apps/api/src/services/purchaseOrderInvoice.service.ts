import purchaseOrderDao from '../dao/purchaseOrder.dao';
import purchaseOrderInvoiceDao from '../dao/purchaseOrderInvoice.dao';
import { purchaseOrderInvoiceBody } from '../interfaces/purchaseOrderInvoice.interface';
import prisma from '../utils/prisma';

/**
 * Method to Create a New PurchaseOrderInvoice
 * @param body
 * @returns
 */
const createPurchaseOrderInvoice = async (body: purchaseOrderInvoiceBody) => {
  try {
    const {
      purchase_order_id,
      grn_id,
      invoice_number,
      invoice_document,
      requested_by,
      invoice_date,
      due_date,
      status,
      additional_info,
      total_amount,
      paid_by,
      paid_date,
      created_by,
    } = body;
    const purchaseOrderInvoiceDetails = await purchaseOrderInvoiceDao.add(
      purchase_order_id,
      grn_id,
      invoice_number,
      invoice_document,
      requested_by,
      invoice_date,
      due_date,
      status,
      additional_info,
      total_amount,
      paid_by,
      paid_date,
      created_by
    );
    const result = {
      message: 'success',
      status: true,
      data: purchaseOrderInvoiceDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in purchaseOrderInvoice service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing PurchaseOrderInvoice
 * @param body
 * @returns
 */

const updatePurchaseOrderInvoice = async (body: purchaseOrderInvoiceBody) => {
  try {
    const {
      purchase_order_id,
      grn_id,
      invoice_number,
      invoice_document,
      requested_by,
      invoice_date,
      due_date,
      status,
      additional_info,
      total_amount,
      paid_by,
      paid_date,
      updated_by,
      purchase_order_invoice_id,
    } = body;
    let result = null;
    const purchaseOrderInvoiceExist = await purchaseOrderInvoiceDao.getById(
      purchase_order_invoice_id
    );
    if (!purchaseOrderInvoiceExist) {
      result = {
        message: 'purchase_order_invoice_id does not exist',
        status: false,
        data: null,
      };
      return result;
    } else {
      const purchaseOrderInvoiceDetails = await purchaseOrderInvoiceDao.edit(
        purchase_order_id,
        grn_id,
        invoice_number,
        invoice_document,
        requested_by,
        invoice_date,
        due_date,
        status,
        additional_info,
        total_amount,
        paid_by,
        paid_date,
        updated_by,
        purchase_order_invoice_id
      );
      result = {
        message: 'success',
        status: true,
        data: purchaseOrderInvoiceDetails,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in purchaseOrderInvoice service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get PurchaseOrderInvoice By PurchaseOrderInvoiceId
 * @param purchaseOrderInvoiceId
 * @returns
 */
const getById = async (purchaseOrderInvoiceId: number) => {
  try {
    let result = null;
    const purchaseOrderInvoiceData = await purchaseOrderInvoiceDao.getById(
      purchaseOrderInvoiceId
    );
    if (purchaseOrderInvoiceData) {
      result = {
        message: 'success',
        status: true,
        data: purchaseOrderInvoiceData,
      };
      return result;
    } else {
      result = {
        message: 'purchase_order_invoice_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getById purchaseOrderInvoice service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get PurchaseOrderInvoice By PurchaseOrderId
 * @param PurchaseOrderId
 * @returns
 */
const getByPOId = async (purchaseOrderId: number) => {
  try {
    const purchaseOrderExist = await purchaseOrderDao.getById(purchaseOrderId);
    if (!purchaseOrderExist) {
      return {
        message: 'purchase_order_id does not exist',
        status: false,
        data: null,
      };
    }

    const purchaseOrderInvoiceData = await purchaseOrderInvoiceDao.getByPOId(
      purchaseOrderId
    );
    if (purchaseOrderInvoiceData.length > 0) {
      return {
        message: 'success',
        status: true,
        data: purchaseOrderInvoiceData,
      };
    } else {
      return {
        message: 'No data found for this purchase_order_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in getByPOId purchaseOrderInvoice service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All PurchaseOrderInvoices
 * @returns
 */
const getAllPurchaseOrderInvoices = async () => {
  try {
    const result = await purchaseOrderInvoiceDao.getAll();
    const purchaseOrderInvoiceData = {
      message: 'success',
      status: true,
      data: result,
    };
    return purchaseOrderInvoiceData;
  } catch (error) {
    console.log(
      'Error occurred in getAllPurchaseOrderInvoices purchaseOrderInvoice service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete purchaseOrderInvoice
 * @param purchaseOrderInvoiceId
 */
const deletePurchaseOrderInvoice = async (purchaseOrderInvoiceId: number) => {
  try {
    const purchaseOrderInvoiceExist = await purchaseOrderInvoiceDao.getById(
      purchaseOrderInvoiceId
    );

    if (!purchaseOrderInvoiceExist) {
      const result = {
        message: 'purchase_order_invoice_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await purchaseOrderInvoiceDao.deletePurchaseOrderInvoice(
      purchaseOrderInvoiceId
    );
    if (data) {
      const result = {
        message: 'PurchaseOrderInvoice Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this purchaseOrderInvoice',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deletePurchaseOrderInvoice purchaseOrderInvoice service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search PurchaseOrderInvoice - Pagination API
 * @returns
 */
const searchPurchaseOrderInvoice = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const purchase_order_id = body.purchase_order_id;
    const global_search = body.global_search;
    const project_id = body.project_id;
    const filterObj: any = {};

    if (purchase_order_id) {
      filterObj.filterPurchaseOrderInvoice =
        filterObj.filterPurchaseOrderInvoice || {};
      filterObj.filterPurchaseOrderInvoice.AND =
        filterObj.filterPurchaseOrderInvoice.AND || [];

      filterObj.filterPurchaseOrderInvoice.AND.push({
        purchase_order_id: purchase_order_id,
      });
    }

    if (project_id) {
      filterObj.filterPurchaseOrderInvoice =
        filterObj.filterPurchaseOrderInvoice || {};
      filterObj.filterPurchaseOrderInvoice.AND =
        filterObj.filterPurchaseOrderInvoice.AND || [];

      filterObj.filterPurchaseOrderInvoice.AND.push({
        grn_data: {
          project_id: project_id,
        },
      });
    }

    if (global_search) {
      filterObj.filterPurchaseOrderInvoice =
        filterObj.filterPurchaseOrderInvoice || {};
      filterObj.filterPurchaseOrderInvoice.OR =
        filterObj.filterPurchaseOrderInvoice.OR || [];

      filterObj.filterPurchaseOrderInvoice.OR.push(
        {
          status: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          invoice_number: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          requested_by_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          requested_by_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          paid_by_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          paid_by_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await purchaseOrderInvoiceDao.searchPurchaseOrderInvoice(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );
    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    if (result.count >= 0) {
      const tempPurchaseOrderInvoiceData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempPurchaseOrderInvoiceData;
    } else {
      const tempPurchaseOrderInvoiceData = {
        message: 'No data found',
        status: false,
        is_available: false,
      };
      return tempPurchaseOrderInvoiceData;
    }
  } catch (error) {
    console.log(
      'Error occurred in searchPurchaseOrderInvoice PurchaseOrderInvoice service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Update an Existing PurchaseOrderInvoice's Status
 * @param body
 * @returns
 */
const updateStatus = async (body: purchaseOrderInvoiceBody) => {
  try {
    const {
      purchase_order_id,
      status,
      paid_by,
      paid_date,
      updated_by,
      payment_mode,
      additional_info,
      purchase_order_invoice_id,
    } = body;
    const purchaseOrderInvoiceExist = await purchaseOrderInvoiceDao.getById(
      purchase_order_invoice_id
    );
    if (!purchaseOrderInvoiceExist) {
      return {
        message: 'purchase_order_invoice_id does not exist',
        status: false,
        data: null,
      };
    }
    const result = await prisma
      .$transaction(
        async (tx) => {
          const purchaseOrderInvoiceDetails =
            await purchaseOrderInvoiceDao.updateStatus(
              purchase_order_id,
              status,
              paid_by,
              paid_date,
              updated_by,
              payment_mode,
              additional_info,
              purchase_order_invoice_id,
              tx
            );

          let allPaid = false;
          const purchaseOrderInvoiceDetailsByPOId =
            await purchaseOrderInvoiceDao.getByPOId(purchase_order_id, tx);
          for await (const data of purchaseOrderInvoiceDetailsByPOId) {
            const purchase_order_status = data.status;
            if (purchase_order_status === 'Paid') {
              allPaid = true;
            }
          }
          if (allPaid === true) {
            await purchaseOrderDao.updateStatusByPOId(
              'Completed',
              updated_by,
              purchase_order_id,
              tx
            );
          }

          const result = {
            message: 'success',
            status: true,
            data: purchaseOrderInvoiceDetails,
          };
          return result;
        },
        {
          timeout: Number(process.env.TRANSACTION_TIMEOUT),
        }
      )
      .then(async (data) => {
        console.log('Successfully Purchase Order Invoice Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderInvoice service updateStatus: ',
      error
    );
    throw error;
  }
};

export {
  createPurchaseOrderInvoice,
  updatePurchaseOrderInvoice,
  getAllPurchaseOrderInvoices,
  getById,
  getByPOId,
  deletePurchaseOrderInvoice,
  searchPurchaseOrderInvoice,
  updateStatus,
};
