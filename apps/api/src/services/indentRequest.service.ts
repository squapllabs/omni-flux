import indentRequestDao from '../dao/indentRequest.dao';
import projectDao from '../dao/project.dao';
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
      project_id,
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
      indent_request_details,
      project_id
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
      project_id,
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
      project_id,
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
    const project_id = body.project_id;
    const approvar_user_id = body.approvar_user_id;
    const approvar_status = body.approvar_status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterIndentRequest = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (project_id) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        project_id: project_id,
      });
    }

    if (approvar_user_id) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        approvar_user_id: approvar_user_id,
      });
    }

    if (approvar_status) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        approvar_status: approvar_status,
      });
    }

    if (global_search) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.OR = filterObj.filterIndentRequest.OR || [];

      filterObj.filterIndentRequest.OR.push(
        {
          request_status: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          approvar_status: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          description: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          priority: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          approvar_comments: {
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
          approvar_user_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          approvar_user_data: {
            last_name: {
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
          indent_request_details: {
            some: {
              bom_detail_data: {
                bom_name: {
                  contains: global_search,
                  mode: 'insensitive',
                },
              },
            },
          },
        },
        {
          indent_request_details: {
            some: {
              bom_detail_data: {
                uom_data: {
                  name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        },
        {
          indent_request_details: {
            some: {
              bom_detail_data: {
                sub_category_data: {
                  name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        },
        {
          indent_request_details: {
            some: {
              bom_detail_data: {
                item_data: {
                  item_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        },
        {
          indent_request_details: {
            some: {
              bom_detail_data: {
                machinery_data: {
                  machinery_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        },
        {
          indent_request_details: {
            some: {
              bom_detail_data: {
                labour_data: {
                  labour_type: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        }
      );
    }

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

/**
 * Method to get IndentRequest By project_id
 * @param project_id
 * @returns
 */
const getByProjectId = async (project_id: number) => {
  try {
    let result = null;
    const projectExist = await projectDao.getById(project_id);
    if (!projectExist) {
      return {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
    }
    const indentRequestData = await indentRequestDao.getByProjectId(project_id);
    if (indentRequestData.length > 0) {
      result = { message: 'success', status: true, data: indentRequestData };
      return result;
    } else {
      result = {
        message: 'No data found related to this project_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectId indentRequest service : ',
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
  getByProjectId,
};