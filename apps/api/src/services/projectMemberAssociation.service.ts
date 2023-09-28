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
    const project_role_id = body.project_role_id;
    const approver_status = body.approver_status;

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

    if (project_role_id) {
      filterObj.filterProjectMemberAssociation.AND =
        filterObj.filterProjectMemberAssociation.AND || [];
      filterObj.filterProjectMemberAssociation.AND.push({
        project_role_id: project_role_id,
      });
    }

    if (approver_status) {
      filterObj.approver_status = approver_status;
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

/**
 * Method to get projectMemberAssociation By ProjectId And UserId
 * @param user_id
 * @param project_role_id
 * @returns
 */
const getByUserIdAndProjectRoleId = async (
  user_id: number,
  project_role_id: number
) => {
  try {
    let result = null;

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
      await projectMemberAssociationDao.getByUserIdAndProjectRoleId(
        user_id,
        project_role_id
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
        message: 'This user_id and project_role_id combination does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByUserIdAndProjectRoleId projectMemberAssociation service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Project Member Association - Pagination API
 * @returns
 */
const searchByUserId = async (body) => {
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
    const project_status = body.project_status;
    const user_id = body.user_id;
    let result = null;

    const filterObj: any = {};

    if (status) {
      filterObj.filterProjectMemberAssociation = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (user_id) {
      filterObj.filterProjectMemberAssociation.AND =
        filterObj.filterProjectMemberAssociation.AND || [];
      filterObj.filterProjectMemberAssociation.AND.push({
        user_id: user_id,
      });
    }

    if (project_status) {
      filterObj.filterProjectMemberAssociation.AND =
        filterObj.filterProjectMemberAssociation.AND || [];
      filterObj.filterProjectMemberAssociation.AND.push({
        project_data: { status: project_status },
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
          project_data: {
            description: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_data: {
            status: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_data: {
            code: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_data: {
            project_notes: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_data: {
            project_type: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_data: {
            client: {
              name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          project_data: {
            client: {
              name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          project_data: {
            user: {
              first_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          project_data: {
            user: {
              last_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        }
      );
    }

    if (!user_id) {
      const filterObj: any = {};

      if (project_status) {
        filterObj.filterProject = filterObj.filterProject || {};
        filterObj.filterProject.AND = filterObj.filterProject.AND || [];
        filterObj.filterProject.AND.push({
          status: project_status,
        });
      }

      if (global_search) {
        filterObj.filterProject = filterObj.filterProject || {};
        filterObj.filterProject.OR = filterObj.filterProject.OR || [];

        filterObj.filterProject.OR.push(
          { project_name: { contains: global_search, mode: 'insensitive' } },
          { description: { contains: global_search, mode: 'insensitive' } },
          { status: { contains: global_search, mode: 'insensitive' } },
          { project_notes: { contains: global_search, mode: 'insensitive' } },
          { project_type: { contains: global_search, mode: 'insensitive' } },
          { code: { contains: global_search, mode: 'insensitive' } },
          {
            client: {
              name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
          {
            user: {
              first_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
          {
            user: {
              last_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          }
        );

        filterObj.filterProject.OR.push({
          OR: [
            {
              project_site: {
                some: {
                  status: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
            {
              project_site: {
                some: {
                  site_details: {
                    name: {
                      contains: global_search,
                      mode: 'insensitive',
                    },
                  },
                },
              },
            },
          ],
        });
      }

      result = await projectDao.searchProject(
        offset,
        limit,
        order_by_column,
        order_by_direction,
        filterObj
      );
    } else {
      const data =
        await projectMemberAssociationDao.searchProjectMemberAssociation(
          offset,
          limit,
          order_by_column,
          order_by_direction,
          filterObj
        );

      const project_data = [];

      await data.data.map((data) => {
        const project = data.project_data;

        project_data.push(project);
      });

      result = { count: data?.count, data: project_data };
    }

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
      'Error occurred in searchByUserId ProjectMemberAssociation service : ',
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
  getByUserIdAndProjectRoleId,
  searchByUserId,
};
