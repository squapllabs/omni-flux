import indentRequestDao from '../dao/indentRequest.dao';
import notificationDao from '../dao/notification.dao';
import projectDao from '../dao/project.dao';
import siteContractorDao from '../dao/siteContractor.dao';
import userDao from '../dao/user.dao';
import { indentRequestBody } from '../interfaces/indentRequest.interface';
import prisma from '../utils/prisma';

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
      approver_user_id,
      approver_status,
      approved_date,
      rejected_date,
      approver_comments,
      created_by,
      indent_request_details,
      project_id,
      site_id,
      request_type,
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

    if (approver_user_id) {
      const approvarUserExist = await userDao.getById(approver_user_id);
      if (!approvarUserExist) {
        return {
          message: 'approver_user_id does not exist',
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

    if (site_id) {
      const siteExist = await siteContractorDao.getBySiteId(site_id);
      if (!siteExist) {
        return {
          message: 'site_id does not exist',
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
      approver_user_id,
      approver_status,
      approved_date,
      rejected_date,
      approver_comments,
      created_by,
      site_id,
      indent_request_details,
      project_id,
      request_type
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
      approver_user_id,
      approver_status,
      approved_date,
      rejected_date,
      approver_comments,
      updated_by,
      indent_request_details,
      indent_request_id,
      project_id,
      site_id,
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

    if (approver_user_id) {
      const approvarUserExist = await userDao.getById(approver_user_id);
      if (!approvarUserExist) {
        return {
          message: 'approver_user_id does not exist',
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

    if (site_id) {
      const siteExist = await siteContractorDao.getBySiteId(site_id);
      if (!siteExist) {
        return {
          message: 'site_id does not exist',
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
      approver_user_id,
      approver_status,
      approved_date,
      rejected_date,
      approver_comments,
      updated_by,
      indent_request_details,
      project_id,
      site_id,
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
    const approver_user_id = body.approver_user_id;
    const approver_status = body.approver_status;
    const priority = body.priority;
    const project_approver_id = body.project_approver_id;
    const indent_request_code = body.indent_request_code;
    const request_type = body.request_type;

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

    if (approver_user_id) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        approver_user_id: approver_user_id,
      });
    }

    if (approver_status) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        approver_status: approver_status,
      });
    }

    if (priority) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        priority: {
          equals: priority,
        },
      });
    }

    if (project_approver_id) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        project_data: {
          approvar_id: project_approver_id,
        },
      });
    }

    if (indent_request_code) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        indent_request_code: {
          contains: indent_request_code,
          mode: 'insensitive',
        },
      });
    }

    if (request_type) {
      filterObj.filterIndentRequest = filterObj.filterIndentRequest || {};
      filterObj.filterIndentRequest.AND =
        filterObj.filterIndentRequest.AND || [];

      filterObj.filterIndentRequest.AND.push({
        request_type: request_type,
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
          approver_status: {
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
          approver_comments: {
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
          approver_user_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          approver_user_data: {
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
          site_data: {
            name: {
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
        },
        {
          request_type: {
            contains: global_search,
            mode: 'insensitive',
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

    const count = result?.count;
    const data = result?.data;

    const total_pages = count < limit ? 1 : Math.ceil(count / limit);

    if (result?.count >= 0) {
      const tempIndentRequestData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempIndentRequestData;
    } else {
      const tempIndentRequestData = {
        message: 'No data found',
        status: false,
        is_available: false,
      };
      return tempIndentRequestData;
    }
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

/**
 * Method to Update Indent Request Status With Comments
 * @param body
 * @returns
 */
const updateStatus = async (body) => {
  try {
    const {
      approver_status,
      approved_date,
      rejected_date,
      approver_comments,
      updated_by,
      indent_request_id,
      approver_user_id,
      request_type,
    } = body;

    const indentRequestExist = await indentRequestDao.getById(
      indent_request_id
    );
    if (!indentRequestExist) {
      return {
        message: 'indent_request_id does not exist',
        status: false,
        data: null,
      };
    }

    const result = await prisma
      .$transaction(async (prisma) => {
        const indentRequest = await indentRequestDao.updateStatus(
          indent_request_id,
          approver_status,
          approver_comments,
          approved_date,
          rejected_date,
          updated_by,
          approver_user_id,
          request_type,
          prisma
        );

        await notificationDao.add(
          approver_user_id,
          indentRequestExist?.requester_user_id,
          indent_request_id,
          `Indent-${approver_status}`,
          `Notification to indent_requestor_user_id as Indent ${approver_status}`,
          updated_by,
          prisma
        );

        return {
          message: 'success',
          status: true,
          data: indentRequest,
        };
      })
      .then((data) => {
        console.log('Successfully Indent Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log(
      'Error occurred in updateStatus indentRequest service : ',
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
  updateStatus,
};
