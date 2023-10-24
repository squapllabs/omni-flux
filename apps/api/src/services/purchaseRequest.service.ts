import indentRequestDao from '../dao/indentRequest.dao';
import projectDao from '../dao/project.dao';
import purchaseRequestDao from '../dao/purchaseRequest.dao';
import userDao from '../dao/user.dao';
import vendorDao from '../dao/vendor.dao';
import { purchaseRequestBody } from '../interfaces/purchaseRequest.interface';
import prisma from '../utils/prisma';
import s3 from '../utils/s3';
import fs from 'fs';
import mailService from './mail.service';
import indentRequestDetailsDao from '../dao/indentRequestDetails.dao';

/**
 * Method to Create a New PurchaseRequest
 * @param body
 * @returns
 */
const createPurchaseRequest = async (body: purchaseRequestBody) => {
  try {
    const {
      indent_request_id,
      requester_user_id,
      request_date,
      status,
      vendor_selection_method,
      project_id,
      selected_vendor_id,
      total_cost,
      created_by,
      vendor_ids,
      purchase_request_details,
      purchase_request_documents,
    } = body;

    let indentRequestExist = null;
    if (indent_request_id) {
      indentRequestExist = await indentRequestDao.getById(indent_request_id);
      if (!indentRequestExist) {
        return {
          message: 'indent_request_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (requester_user_id) {
      const requesterUserExist = await userDao.getById(requester_user_id);
      if (!requesterUserExist) {
        return {
          message: 'requester_user_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (selected_vendor_id) {
      const vendorExist = await vendorDao.getById(selected_vendor_id);
      if (!vendorExist) {
        return {
          message: 'selected_vendor_id does not exist',
          status: false,
          data: null,
        };
      }
    }
    const result = await prisma
      .$transaction(async (tx) => {
        const purchaseRequestDetails = await purchaseRequestDao.add(
          indent_request_id,
          requester_user_id,
          request_date,
          status,
          vendor_selection_method,
          project_id,
          indentRequestExist?.site_id,
          selected_vendor_id,
          total_cost,
          created_by,
          vendor_ids,
          purchase_request_details,
          purchase_request_documents,
          tx
        );

        if (purchase_request_details) {
          for await (const value of purchase_request_details) {
            const indent_requested_quantity = value.indent_requested_quantity;
            const purchase_requested_quantity =
              value.purchase_requested_quantity;
            const indent_request_details_id = value.indent_request_details_id;
            const purchase_remaining_quantity = Math.max(
              0,
              indent_requested_quantity - purchase_requested_quantity
            );
            await indentRequestDetailsDao.updatePurchaseRequestQuantity(
              indent_request_details_id,
              purchase_requested_quantity,
              purchase_remaining_quantity,
              created_by,
              tx
            );
          }
        }

        const result = {
          message: 'success',
          status: true,
          data: purchaseRequestDetails,
        };
        return result;
      })
      .then(async (data) => {
        console.log('Successfully Purchase Request Data Returned ', data);
        const emailData = [];
        for (const vendor of vendor_ids) {
          const vendor_id = vendor;
          const vendorDetails = await vendorDao.getById(vendor_id);

          const vendorEmailBody = {
            purchase_request_details: purchase_request_details,
            vendor_name: vendorDetails.vendor_name,
            to_email_id: vendorDetails.contact_email,
          };
          emailData.push(vendorEmailBody);
        }
        for (const email of emailData) {
          await mailService.purchaseRequestEmailForVendor(email);
        }
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in purchaseRequest service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing PurchaseRequest
 * @param body
 * @returns
 */

const updatePurchaseRequest = async (req) => {
  try {
    const body = req.body;
    const files = req.files.purchase_request_documents;
    const {
      indent_request_id,
      requester_user_id,
      request_date,
      status,
      vendor_selection_method,
      project_id,
      selected_vendor_id,
      total_cost,
      updated_by,
      purchase_request_details,
      purchase_request_id,
    } = body;
    let result = null;
    const purchaseRequestExist = await purchaseRequestDao.getById(
      purchase_request_id
    );
    if (!purchaseRequestExist) {
      result = {
        message: 'purchase_request_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    let indentRequestExist = null;
    if (indent_request_id) {
      indentRequestExist = await indentRequestDao.getById(indent_request_id);
      if (!indentRequestExist) {
        return {
          message: 'indent_request_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (requester_user_id) {
      const requesterUserExist = await userDao.getById(requester_user_id);
      if (!requesterUserExist) {
        return {
          message: 'requester_user_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (selected_vendor_id) {
      const vendorExist = await vendorDao.getById(selected_vendor_id);
      if (!vendorExist) {
        return {
          message: 'selected_vendor_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    /* Purchase Request Document File Handling */

    const purchaseRequestDocuments = [];
    let index = 0;
    if (files) {
      const existingDocument = purchaseRequestExist?.purchase_request_documents;

      if (existingDocument?.length > 0) {
        for (const doc of existingDocument) {
          const existingDocPath = doc.path;
          await s3.deleteFileFromS3UsingPath(existingDocPath);
          console.log('Existing File deleted successfully.');
        }
      }

      for (const file of files) {
        const contentType = file.mimetype;
        const code = `purchase-request-${purchase_request_id}-${index}`;
        const localUploadedFilePath = file.path;
        const fileData = await s3.uploadFileInS3(
          file,
          code,
          'purchase-request',
          contentType
        );

        const s3FilePath = fileData.path;

        fs.unlink(localUploadedFilePath, (err) => {
          if (err) {
            console.error('Error deleting the local file:', err);
          } else {
            console.log('Local File deleted successfully.');
          }
        });

        purchaseRequestDocuments.push({
          index,
          path: s3FilePath,
          folder: 'purchase-request',
          code: code,
        });
        index++;
      }
    }

    const purchaseRequestDetails = await purchaseRequestDao.edit(
      Number(indent_request_id),
      Number(requester_user_id),
      request_date,
      status,
      vendor_selection_method,
      Number(project_id),
      indentRequestExist?.site_id,
      Number(selected_vendor_id),
      Number(total_cost),
      Number(updated_by),
      JSON.parse(purchase_request_details),
      purchaseRequestDocuments,
      Number(purchase_request_id)
    );
    result = {
      message: 'success',
      status: true,
      data: purchaseRequestDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in purchaseRequest service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get PurchaseRequest By PurchaseRequestId
 * @param purchaseRequestId
 * @returns
 */
const getById = async (purchaseRequestId: number) => {
  try {
    let result = null;
    const purchaseRequestData = await purchaseRequestDao.getById(
      purchaseRequestId
    );
    if (purchaseRequestData) {
      result = { message: 'success', status: true, data: purchaseRequestData };
      return result;
    } else {
      result = {
        message: 'purchase_request_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById purchaseRequest service : ', error);
    throw error;
  }
};

/**
 * Method to Get All PurchaseRequests
 * @returns
 */
const getAllPurchaseRequests = async () => {
  try {
    const result = await purchaseRequestDao.getAll();
    const purchaseRequestData = {
      message: 'success',
      status: true,
      data: result,
    };
    return purchaseRequestData;
  } catch (error) {
    console.log(
      'Error occurred in getAllPurchaseRequests purchaseRequest service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete purchaseRequest
 * @param purchaseRequestId
 */
const deletePurchaseRequest = async (purchaseRequestId: number) => {
  try {
    const purchaseRequestExist = await purchaseRequestDao.getById(
      purchaseRequestId
    );

    if (!purchaseRequestExist) {
      const result = {
        message: 'purchase_request_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await purchaseRequestDao.deletePurchaseRequest(
      purchaseRequestId
    );
    if (data) {
      const result = {
        message: 'PurchaseRequest Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this purchaseRequest',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deletePurchaseRequest purchaseRequest service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search PurchaseRequest - Pagination API
 * @returns
 */
const searchPurchaseRequest = async (body) => {
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
    const indent_request_id = body.indent_request_id;
    const purchase_request_status = body.purchase_request_status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterPurchaseRequest = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (indent_request_id) {
      filterObj.filterPurchaseRequest = filterObj.filterPurchaseRequest || {};
      filterObj.filterPurchaseRequest.AND =
        filterObj.filterPurchaseRequest.AND || [];
      filterObj.filterPurchaseRequest.AND.push({
        indent_request_id: indent_request_id,
      });
    }

    if (purchase_request_status) {
      filterObj.filterPurchaseRequest = filterObj.filterPurchaseRequest || {};
      filterObj.filterPurchaseRequest.AND =
        filterObj.filterPurchaseRequest.AND || [];
      filterObj.filterPurchaseRequest.AND.push({
        status: purchase_request_status,
      });
    }

    if (global_search) {
      filterObj.filterPurchaseRequest = filterObj.filterPurchaseRequest || {};
      filterObj.filterPurchaseRequest.OR =
        filterObj.filterPurchaseRequest.OR || [];

      filterObj.filterPurchaseRequest.OR.push(
        {
          status: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          vendor_selection_method: {
            contains: global_search,
            mode: 'insensitive',
          },
        },

        {
          requester_user_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          requester_user_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          indent_request_data: {
            description: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },

        {
          project_data: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          selected_vendor_data: {
            vendor_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await purchaseRequestDao.searchPurchaseRequest(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempPurchaseRequestData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempPurchaseRequestData;
  } catch (error) {
    console.log(
      'Error occurred in searchPurchaseRequest PurchaseRequest service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get All Purchase Request Projects By Status
 * @param status
 * @returns
 */
const getAllPurchaseRequestProjectsByStatus = async (status: string) => {
  try {
    let result = null;
    const purchaseRequestData =
      await purchaseRequestDao.getAllPurchaseRequestProjectsByStatus(status);
    if (purchaseRequestData.length > 0) {
      result = { message: 'success', status: true, data: purchaseRequestData };
      return result;
    } else {
      result = {
        message: 'No projects found under this status',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getAllPurchaseRequestProjectsByStatus purchaseRequest service : ',
      error
    );
    throw error;
  }
};

export {
  createPurchaseRequest,
  updatePurchaseRequest,
  getAllPurchaseRequests,
  getById,
  deletePurchaseRequest,
  searchPurchaseRequest,
  getAllPurchaseRequestProjectsByStatus,
};
