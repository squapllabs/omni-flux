import vendorQuotesDao from '../dao/vendorQuotes.dao';
import vendorDao from '../dao/vendor.dao';
import { vendorQuotesBody } from '../interfaces/vendorQuotes.interface';
import purchaseRequestDao from '../dao/purchaseRequest.dao';
import { processFileDeleteInS3 } from '../utils/fileUpload';
import prisma from '../utils/prisma';
import vendorQuotationDetailsDao from '../dao/vendorQuotationDetails.dao';
import purchaseRequestQuotationDetailsDao from '../dao/purchaseRequestQuotationDetails.dao';

/**
 * Method to Create a New VendorQuotes
 * @param body
 * @returns
 */
const createVendorQuotes = async (body: vendorQuotesBody) => {
  try {
    const {
      vendor_id,
      purchase_request_id,
      quotation_date,
      quotation_status,
      total_quotation_amount,
      remarks,
      created_by,
      vendor_quotes_documents,
    } = body;

    if (purchase_request_id) {
      const purchaseRequesterExist = await purchaseRequestDao.getById(
        purchase_request_id
      );
      if (!purchaseRequesterExist) {
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

    const vendorQuotesDetails = await vendorQuotesDao.add(
      vendor_id,
      purchase_request_id,
      quotation_date,
      quotation_status,
      total_quotation_amount,
      remarks,
      vendor_quotes_documents,
      created_by
    );
    const result = {
      message: 'success',
      status: true,
      data: vendorQuotesDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in VendorQuotes service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing VendorQuotes
 * @param body
 * @returns
 */

const updateVendorQuotes = async (body: vendorQuotesBody) => {
  try {
    const {
      vendor_quotes_id,
      vendor_id,
      purchase_request_id,
      quotation_date,
      quotation_status,
      total_quotation_amount,
      remarks,
      vendor_quotes_documents,
      updated_by,
      vendor_quotation_details,
    } = body;
    let result = null;
    const vendorQuotesExist = await vendorQuotesDao.getById(vendor_quotes_id);
    if (!vendorQuotesExist) {
      result = {
        message: 'vendor_quotes_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (purchase_request_id) {
      const purchaseRequesterExist = await purchaseRequestDao.getById(
        purchase_request_id
      );
      if (!purchaseRequesterExist) {
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

    const updatedVendorQuotesDocuments = [];
    if (vendor_quotes_documents) {
      for (const doc of vendor_quotes_documents) {
        const { is_delete, path } = doc;

        if (is_delete === true) {
          const deleteDocInS3Body = {
            path,
          };
          await processFileDeleteInS3(deleteDocInS3Body);
        } else {
          updatedVendorQuotesDocuments.push(doc);
        }
      }
    }
    const vendorQuotesData = await prisma
      .$transaction(async (tx) => {
        const vendorQuotesDetails = await vendorQuotesDao.edit(
          vendor_quotes_id,
          vendor_id,
          purchase_request_id,
          quotation_date,
          quotation_status,
          total_quotation_amount,
          remarks,
          updated_by,
          updatedVendorQuotesDocuments,
          tx
        );
        const vendorQuotationDetailsData = [];
        if (vendor_quotation_details) {
          for await (const vendor_quotation_detail of vendor_quotation_details) {
            const vendor_quotation_details_id =
              vendor_quotation_detail.vendor_quotation_details_id;
            const item_id = vendor_quotation_detail.item_id;
            const purchase_requested_quantity =
              vendor_quotation_detail.purchase_requested_quantity;
            const indent_requested_quantity =
              vendor_quotation_detail.indent_requested_quantity;
            const indent_request_details_id =
              vendor_quotation_detail.indent_request_details_id;
            const unit_cost = vendor_quotation_detail.unit_cost;
            const total_cost = vendor_quotation_detail.total_cost;

            const vendorQuotationDetails = await vendorQuotationDetailsDao.edit(
              vendor_quotes_id,
              item_id,
              indent_request_details_id,
              indent_requested_quantity,
              purchase_requested_quantity,
              unit_cost,
              total_cost,
              updated_by,
              vendor_quotation_details_id,
              tx
            );
            vendorQuotationDetailsData.push(vendorQuotationDetails);
          }
        }

        if (quotation_status === 'Approved') {
          await purchaseRequestDao.updateVendor(
            quotation_status,
            vendor_id,
            updated_by,
            total_quotation_amount,
            updatedVendorQuotesDocuments,
            purchase_request_id,
            tx
          );

          const vendorQuotationDetailsData =
            await vendorQuotationDetailsDao.getByVendorQuotesId(
              vendor_quotes_id
            );

          for await (const vendorQuotationDetail of vendorQuotationDetailsData) {
            const item_id = vendorQuotationDetail.item_id;
            const purchase_requested_quantity =
              vendorQuotationDetail.purchase_requested_quantity;
            const indent_requested_quantity =
              vendorQuotationDetail.indent_requested_quantity;
            const indent_request_details_id =
              vendorQuotationDetail.indent_request_details_id;
            const unit_cost = vendorQuotationDetail.unit_cost;
            const total_cost = vendorQuotationDetail.total_cost;

            await purchaseRequestQuotationDetailsDao.add(
              purchase_request_id,
              item_id,
              indent_request_details_id,
              indent_requested_quantity,
              purchase_requested_quantity,
              unit_cost,
              total_cost,
              updated_by,
              tx
            );
          }

          const getVendorQuotesByPurchaseRequestId =
            await vendorQuotesDao.getByPurchaseRequestId(
              purchase_request_id,
              tx
            );

          for await (const vendorQuotes of getVendorQuotesByPurchaseRequestId) {
            const new_vendor_quotes_id = vendorQuotes?.vendor_quotes_id;
            if (new_vendor_quotes_id !== vendor_quotes_id) {
              await vendorQuotesDao.updateStatus(
                new_vendor_quotes_id,
                'Rejected',
                updated_by,
                tx
              );
            }
          }
        }

        result = {
          message: 'success',
          status: true,
          data: vendorQuotesDetails,
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
    return vendorQuotesData;
  } catch (error) {
    console.log('Error occurred in VendorQuotes service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get VendorQuotes By VendorQuotesId
 * @param VendorQuotesId
 * @returns
 */
const getById = async (vendorQuotesId: number) => {
  try {
    let result = null;
    const vendorQuotesData = await vendorQuotesDao.getById(vendorQuotesId);
    if (vendorQuotesData) {
      result = { message: 'success', status: true, data: vendorQuotesData };
      return result;
    } else {
      result = {
        message: 'vendor_quotes_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById VendorQuotes service : ', error);
    throw error;
  }
};

/**
 * Method to Get All VendorQuotess
 * @returns
 */
const getAllVendorQuotes = async () => {
  try {
    const result = await vendorQuotesDao.getAll();
    const vendorQuotesData = {
      message: 'success',
      status: true,
      data: result,
    };
    return vendorQuotesData;
  } catch (error) {
    console.log(
      'Error occurred in getAllVendorQuotes VendorQuotes service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete VendorQuotes
 * @param VendorQuotesId
 */
const deleteVendorQuotes = async (vendorQuotesId: number) => {
  try {
    const vendorQuotesExist = await vendorQuotesDao.getById(vendorQuotesId);

    if (!vendorQuotesExist) {
      const result = {
        message: 'vendor_quotes_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await vendorQuotesDao.deleteVendorQuotes(vendorQuotesId);
    if (data) {
      const result = {
        message: 'VendorQuotes Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this VendorQuotes',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteVendorQuotes VendorQuotes service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search VendorQuotes - Pagination API
 * @returns
 */
const searchVendorQuotes = async (body) => {
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
    const purchase_request_id = body.purchase_request_id;

    const result = await vendorQuotesDao.searchVendorQuotes(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      global_search,
      status,
      purchase_request_id
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempVendorQuotesData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempVendorQuotesData;
  } catch (error) {
    console.log(
      'Error occurred in searchVendorQuotes VendorQuotes service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Update an Existing Status And Document
 * @param body
 * @returns
 */

const updateStatusAndDocument = async (body: vendorQuotesBody) => {
  try {
    const {
      vendor_quotes_id,
      quotation_status,
      vendor_quotes_documents,
      updated_by,
    } = body;
    let result = null;
    const vendorQuotesExist = await vendorQuotesDao.getById(vendor_quotes_id);
    if (!vendorQuotesExist) {
      result = {
        message: 'vendor_quotes_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const updatedVendorQuotesDocuments = [];
    if (vendor_quotes_documents) {
      for (const doc of vendor_quotes_documents) {
        const { is_delete, path } = doc;

        if (is_delete === true) {
          const deleteDocInS3Body = {
            path,
          };
          await processFileDeleteInS3(deleteDocInS3Body);
        } else {
          updatedVendorQuotesDocuments.push(doc);
        }
      }
    }

    const vendorQuotesDetails = await vendorQuotesDao.updateStatusAndDocument(
      vendor_quotes_id,
      quotation_status,
      updated_by,
      updatedVendorQuotesDocuments
    );
    result = {
      message: 'success',
      status: true,
      data: vendorQuotesDetails,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in VendorQuotes service updateStatusAndDocument: ',
      error
    );
    throw error;
  }
};

/**
 * Method to get VendorQuotes By PurchaseRequest Id And Vendor Id
 * @param purchase_request_id
 * @param vendor_id
 * @returns
 */
const getByPurchaseRequestIdAndVendorId = async (
  purchase_request_id: number,
  vendor_id: number
) => {
  try {
    let result = null;

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

    const vendorExist = await vendorDao.getById(vendor_id);
    if (!vendorExist) {
      return {
        message: 'vendor_id does not exist',
        status: false,
        data: null,
      };
    }

    const vendorQuotesData =
      await vendorQuotesDao.getByPurchaseRequestIdAndVendorId(
        purchase_request_id,
        vendor_id
      );
    if (vendorQuotesData) {
      result = {
        message: 'success',
        status: true,
        is_exist: true,
        data: vendorQuotesData,
      };
      return result;
    } else {
      result = {
        message:
          'No data found for this purchase_request_id and vendor_id combo',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByPurchaseRequestIdAndVendorId VendorQuotes service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get VendorQuotes By PurchaseRequestId
 * @param purchase_request_id
 * @returns
 */
const getByPurchaseRequestId = async (purchase_request_id: number) => {
  try {
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
    const vendorQuotesData = await vendorQuotesDao.getByPurchaseRequestId(
      purchase_request_id
    );
    if (vendorQuotesData.length > 0) {
      return { message: 'success', status: true, data: vendorQuotesData };
    } else {
      return {
        message: 'No data found for this purchase_request_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in getByPurchaseRequestId VendorQuotes service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get Vendor Details By PurchaseRequestId
 * @param purchase_request_id
 * @returns
 */
const getVendorDetailsByPurchaseRequestId = async (
  purchase_request_id: number
) => {
  try {
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
    const vendorQuotesData =
      await vendorQuotesDao.getVendorDetailsByPurchaseRequestId(
        purchase_request_id
      );
    if (vendorQuotesData.length > 0) {
      return { message: 'success', status: true, data: vendorQuotesData };
    } else {
      return {
        message: 'No data found for this purchase_request_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in getByPurchaseRequestId VendorQuotes service : ',
      error
    );
    throw error;
  }
};

export {
  createVendorQuotes,
  updateVendorQuotes,
  getAllVendorQuotes,
  getById,
  deleteVendorQuotes,
  searchVendorQuotes,
  updateStatusAndDocument,
  getByPurchaseRequestIdAndVendorId,
  getByPurchaseRequestId,
  getVendorDetailsByPurchaseRequestId,
};
