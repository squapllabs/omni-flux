import projectDao from '../dao/project.dao';
import projectMemberAssociationDao from '../dao/projectMemberAssociation.dao';
import userDao from '../dao/user.dao';
import { projectMemberAssociationBody } from '../interfaces/projectMemberAssociation.interface';

/**
 * Method to Create a New projectMemberAssociation
 * @param body
 * @returns
 */
const createProjectMemberAssociation = async (
  body: projectMemberAssociationBody
) => {
  try {
    const {
      project_id,
      user_id,
      project_role_id,
      access_start_date,
      access_end_date,
      created_by,
    } = body;

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

    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        return {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (project_id && user_id) {
      const projectUserCombinationExist =
        await projectMemberAssociationDao.getByProjectIdAndUserId(
          project_id,
          user_id
        );
      if (projectUserCombinationExist) {
        return {
          message: 'This project_id and user_id combination already exist',
          status: false,
          data: null,
        };
      }
    }

    const projectMemberAssociationDetails =
      await projectMemberAssociationDao.add(
        project_id,
        user_id,
        project_role_id,
        access_start_date,
        access_end_date,
        created_by
      );
    const result = {
      message: 'success',
      status: true,
      data: projectMemberAssociationDetails,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation service Add: ',
      error
    );
    throw error;
  }
};

/**
 * Method to Update an Existing projectMemberAssociation
 * @param body
 * @returns
 */
const updateProjectMemberAssociation = async (
  body: projectMemberAssociationBody
) => {
  try {
    const {
      project_id,
      user_id,
      project_role_id,
      access_start_date,
      access_end_date,
      updated_by,
      project_member_association_id,
    } = body;
    let result = null;
    const projectMemberAssociationExist =
      await projectMemberAssociationDao.getById(project_member_association_id);

    if (!projectMemberAssociationExist) {
      return {
        message: 'project_member_association_id does not exist',
        status: false,
        data: null,
      };
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

    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        return {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (project_id && user_id) {
      const projectUserCombinationExist =
        await projectMemberAssociationDao.getByProjectIdAndUserId(
          project_id,
          user_id
        );
      if (
        projectUserCombinationExist &&
        projectUserCombinationExist.project_member_association_id !==
          project_member_association_id
      ) {
        return {
          message: 'This project_id and user_id combination already exist',
          status: false,
          data: null,
        };
      }
    }

    const projectMemberAssociationDetails =
      await projectMemberAssociationDao.edit(
        project_id,
        user_id,
        project_role_id,
        access_start_date,
        access_end_date,
        updated_by,
        project_member_association_id
      );
    result = {
      message: 'success',
      status: true,
      data: projectMemberAssociationDetails,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation service Edit: ',
      error
    );
    throw error;
  }
};

/**
 * Method to get projectMemberAssociation By ProjectMemberAssociationId
 * @param projectMemberAssociationId
 * @returns
 */
const getById = async (projectMemberAssociationId: number) => {
  try {
    let result = null;
    const projectMemberAssociationData =
      await projectMemberAssociationDao.getById(projectMemberAssociationId);
    if (projectMemberAssociationData) {
      result = {
        message: 'success',
        status: true,
        data: projectMemberAssociationData,
      };
      return result;
    } else {
      result = {
        message: 'project_member_association_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getById projectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All ProjectMemberAssociations
 * @returns
 */
const getAllProjectMemberAssociation = async () => {
  try {
    const result = await projectMemberAssociationDao.getAll();
    const projectMemberAssociationData = {
      message: 'success',
      status: true,
      data: result,
    };
    return projectMemberAssociationData;
  } catch (error) {
    console.log(
      'Error occurred in getAllProjectMemberAssociation projectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete projectMemberAssociation
 * @param projectMemberAssociationId
 */
const deleteProjectMemberAssociation = async (
  projectMemberAssociationId: number
) => {
  try {
    const projectMemberAssociationExist =
      await projectMemberAssociationDao.getById(projectMemberAssociationId);

    if (!projectMemberAssociationExist) {
      const result = {
        message: 'project_member_association_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data =
      await projectMemberAssociationDao.deleteProjectMemberAssociation(
        projectMemberAssociationId
      );
    if (data) {
      const result = {
        message: 'ProjectMemberAssociation Data Deleted Successfully',
        staus: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this projectMemberAssociation',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteProjectMemberAssociation projectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get projectMemberAssociation By ProjectId And UserId
 * @param project_id
 * @param user_id
 * @returns
 */
const getByProjectIdAndUserId = async (project_id: number, user_id: number) => {
  try {
    let result = null;
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
    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        return {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
      }
    }
    const projectMemberAssociationData =
      await projectMemberAssociationDao.getByProjectIdAndUserId(
        project_id,
        user_id
      );
    if (projectMemberAssociationData) {
      result = {
        message: 'This project_id and user_id combination already exist',
        status: true,
        is_exist: true,
        data: projectMemberAssociationData,
      };
      return result;
    } else {
      result = {
        message: 'This project_id and user_id combination does not exist',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectIdAndUserId projectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get projectMemberAssociation By ProjectId
 * @param project_id
 * @returns
 */
const getByProjectId = async (project_id: number) => {
  try {
    let result = null;
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
    const projectMemberAssociationData =
      await projectMemberAssociationDao.getByProjectId(project_id);
    if (projectMemberAssociationData.length > 0) {
      result = {
        message: 'success',
        status: true,
        data: projectMemberAssociationData,
      };
      return result;
    } else {
      result = {
        message: 'No data found for this project_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectId projectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Project Member Association - Pagination API
 * @returns
 */
const searchProjectMemberAssociation = async (body) => {
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
    const user_id = body.user_id;

    const filterObj: any = {};

    if (status) {
      filterObj.filterProjectMemberAssociation = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (project_id) {
      filterObj.filterProjectMemberAssociation.AND =
        filterObj.filterProjectMemberAssociation.AND || [];
      filterObj.filterProjectMemberAssociation.AND.push({
        project_id: project_id,
      });
    }

    if (user_id) {
      filterObj.filterProjectMemberAssociation.AND =
        filterObj.filterProjectMemberAssociation.AND || [];
      filterObj.filterProjectMemberAssociation.AND.push({
        user_id: user_id,
      });
    }

    if (global_search) {
      filterObj.filterProjectMemberAssociation =
        filterObj.filterProjectMemberAssociation || {};
      filterObj.filterProjectMemberAssociation.OR =
        filterObj.filterProjectMemberAssociation.OR || [];

      filterObj.filterProjectMemberAssociation.OR.push(
        {
          project_data: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          user_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          user_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_role_data: {
            role_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result =
      await projectMemberAssociationDao.searchProjectMemberAssociation(
        offset,
        limit,
        order_by_column,
        order_by_direction,
        filterObj
      );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempProjectData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempProjectData;
  } catch (error) {
    console.log(
      'Error occurred in searchProjectMemberAssociation ProjectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get projectMemberAssociation By ProjectId and Role Name
 * @param project_id
 * @param role_name
 * @returns
 */
const getByProjectIdAndRoleType = async (
  project_id: number,
  role_name: string
) => {
  try {
    let result = null;
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
    const projectMemberAssociationData =
      await projectMemberAssociationDao.getByProjectIdAndRoleType(
        project_id,
        role_name
      );
    if (projectMemberAssociationData.length > 0) {
      result = {
        message: 'success',
        status: true,
        data: projectMemberAssociationData,
      };
      return result;
    } else {
      result = {
        message: 'No data found for this project_id and role_type',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectIdAndRoleType projectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

export {
  createProjectMemberAssociation,
  updateProjectMemberAssociation,
  getAllProjectMemberAssociation,
  getById,
  deleteProjectMemberAssociation,
  getByProjectIdAndUserId,
  getByProjectId,
  searchProjectMemberAssociation,
  getByProjectIdAndRoleType,
};
