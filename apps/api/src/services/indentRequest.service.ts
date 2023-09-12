import indentRequestDao from '../dao/indentRequest.dao';
import userDao from '../dao/user.dao';
import { indentRequestBody } from '../interfaces/indentRequest.interface';

/**
 * Method to Create a New IndentRequest
 * @param body
 * @returns
 */
const createIndentRequest = async (body: indentRequestBody) => {
  try {
    const {
      requester_user_id,
      requested_date,
      request_status,
      priority,
      description,
      expected_delivery_date,
      total_cost,
      approvar_user_id,
      approvar_status,
      approved_date,
      rejected_date,
      approvar_comments,
      created_by,
      indent_request_details,
    } = body;

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

    if (approvar_user_id) {
      const approvarUserExist = await userDao.getById(approvar_user_id);
      if (!approvarUserExist) {
        return {
          message: 'approvar_user_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const indentRequestDetails = await indentRequestDao.add(
      requester_user_id,
      requested_date,
      request_status,
      priority,
      description,
      expected_delivery_date,
      total_cost,
      approvar_user_id,
      approvar_status,
      approved_date,
      rejected_date,
      approvar_comments,
      created_by,
      indent_request_details
    );
    const result = {
      message: 'success',
      status: true,
      data: indentRequestDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in indentRequest service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing IndentRequest
 * @param body
 * @returns
 */

const updateIndentRequest = async (body: indentRequestBody) => {
  try {
    const {
      requester_user_id,
      requested_date,
      request_status,
      priority,
      description,
      expected_delivery_date,
      total_cost,
      approvar_user_id,
      approvar_status,
      approved_date,
      rejected_date,
      approvar_comments,
      updated_by,
      indent_request_details,
      indent_request_id,
    } = body;
    let result = null;
    const indentRequestExist = await indentRequestDao.getById(
      indent_request_id
    );
    if (!indentRequestExist) {
      result = {
        message: 'indent_request_id does not exist',
        status: false,
        data: null,
      };
      return result;
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

    if (approvar_user_id) {
      const approvarUserExist = await userDao.getById(approvar_user_id);
      if (!approvarUserExist) {
        return {
          message: 'approvar_user_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const indentRequestDetails = await indentRequestDao.edit(
      requester_user_id,
      requested_date,
      request_status,
      priority,
      description,
      expected_delivery_date,
      total_cost,
      approvar_user_id,
      approvar_status,
      approved_date,
      rejected_date,
      approvar_comments,
      updated_by,
      indent_request_details,
      indent_request_id
    );
    result = { message: 'success', status: true, data: indentRequestDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in indentRequest service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get IndentRequest By IndentRequestId
 * @param indentRequestId
 * @returns
 */
const getById = async (indentRequestId: number) => {
  try {
    let result = null;
    const indentRequestData = await indentRequestDao.getById(indentRequestId);
    if (indentRequestData) {
      result = { message: 'success', status: true, data: indentRequestData };
      return result;
    } else {
      result = {
        message: 'indent_request_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById indentRequest service : ', error);
    throw error;
  }
};

/**
 * Method to Get All IndentRequests
 * @returns
 */
const getAllIndentRequests = async () => {
  try {
    const result = await indentRequestDao.getAll();
    const indentRequestData = {
      message: 'success',
      status: true,
      data: result,
    };
    return indentRequestData;
  } catch (error) {
    console.log(
      'Error occurred in getAllIndentRequests indentRequest service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete indentRequest
 * @param indentRequestId
 */
const deleteIndentRequest = async (indentRequestId: number) => {
  try {
    const indentRequestExist = await indentRequestDao.getById(indentRequestId);

    if (!indentRequestExist) {
      const result = {
        message: 'indent_request_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await indentRequestDao.deleteIndentRequest(indentRequestId);
    if (data) {
      const result = {
        message: 'IndentRequest Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this indentRequest',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteIndentRequest indentRequest service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search IndentRequest - Pagination API
 * @returns
 */
const searchIndentRequest = async (body) => {
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
    const filterObj = {
      filterIndentRequest: {
        AND: [],
        OR: [
          { name: { contains: global_search, mode: 'insensitive' } },
          { contact_details: { contains: global_search, mode: 'insensitive' } },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await indentRequestDao.searchIndentRequest(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempIndentRequestData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempIndentRequestData;
  } catch (error) {
    console.log(
      'Error occurred in searchIndentRequest IndentRequest service : ',
      error
    );
    throw error;
  }
};

export {
  createIndentRequest,
  updateIndentRequest,
  getAllIndentRequests,
  getById,
  deleteIndentRequest,
  searchIndentRequest,
};
