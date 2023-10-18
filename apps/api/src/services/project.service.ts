import projectDao from '../dao/project.dao';
import userDao from '../dao/user.dao';
import clientDao from '../dao/client.dao';
import {
  createProjectBody,
  updateProjectBody,
} from '../interfaces/project.Interface';
import projectSiteDao from '../dao/projectSite.dao';
import { processFileDeleteInS3 } from '../utils/fileUpload';

/**
 * Method to Create a New Project
 * @param body
 * @returns
 */
const createProject = async (body: createProjectBody) => {
  try {
    const {
      project_name,
      description,
      user_id,
      date_started,
      date_ended,
      status,
      estimated_budget,
      actual_budget,
      code,
      project_type,
      project_notes,
      client_id,
      project_documents,
      created_by,
      site_configuration,
      approvar_id,
      bom_configuration,
    } = body;
    let result = null;

    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        result = {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (client_id) {
      const clientExist = await clientDao.getById(client_id);
      if (!clientExist) {
        result = {
          message: 'client_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (approvar_id) {
      const approvarExist = await userDao.getById(approvar_id);
      if (!approvarExist) {
        result = {
          message: 'approvar_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const projectDetails = await projectDao.add(
      project_name,
      description,
      user_id,
      date_started,
      date_ended,
      status,
      estimated_budget,
      actual_budget,
      code,
      project_type,
      project_notes,
      client_id,
      project_documents,
      created_by,
      site_configuration,
      approvar_id,
      bom_configuration
    );
    result = { message: 'success', status: true, data: projectDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in project service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Project
 * @param body
 * @returns
 */
const updateProject = async (body: updateProjectBody) => {
  try {
    const {
      project_name,
      description,
      user_id,
      date_started,
      date_ended,
      status,
      estimated_budget,
      actual_budget,
      code,
      project_type,
      project_notes,
      client_id,
      project_documents,
      updated_by,
      project_id,
      site_configuration,
      approvar_id,
      bom_configuration,
    } = body;
    let result = null;

    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        result = {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }
    if (client_id) {
      const clientExist = await clientDao.getById(client_id);
      if (!clientExist) {
        result = {
          message: 'client_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (approvar_id) {
      const approvarExist = await userDao.getById(approvar_id);
      if (!approvarExist) {
        result = {
          message: 'approvar_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const updatedProjectDocuments = [] as any;
    if (project_documents) {
      for (const doc of project_documents) {
        const { is_delete, path } = doc;

        if (is_delete === 'Y') {
          const deleteDocInS3Body = {
            path,
          };
          await processFileDeleteInS3(deleteDocInS3Body);
        } else {
          updatedProjectDocuments.push(doc);
        }
      }
    }

    const projectExist = await projectDao.getById(project_id);

    if (projectExist) {
      const projectDetails = await projectDao.edit(
        project_name,
        description,
        user_id,
        date_started,
        date_ended,
        status,
        estimated_budget,
        actual_budget,
        code,
        project_type,
        project_notes,
        client_id,
        updatedProjectDocuments,
        updated_by,
        project_id,
        site_configuration,
        approvar_id,
        bom_configuration
      );
      result = { message: 'success', status: true, data: projectDetails };
      return result;
    } else {
      result = {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in project service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Project By projectId
 * @param projectId
 * @returns
 */
const getById = async (projectId: number) => {
  try {
    let result = null;
    const projectData = await projectDao.getById(projectId);
    if (projectData) {
      result = { message: 'success', status: true, data: projectData };
      return result;
    } else {
      result = {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById project service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Project's
 * @returns
 */
const getAllProject = async () => {
  try {
    const result = await projectDao.getAll();
    const projectData = { message: 'success', status: true, data: result };
    return projectData;
  } catch (error) {
    console.log('Error occurred in getAllProject project service : ', error);
    throw error;
  }
};

/**
 * Method to get dashboard data
 * @returns
 */
const getAllDashboard = async () => {
  try {
    const result = await projectDao.getAllDashboard();
    const dashboardData = { message: 'success', status: true, data: result };
    return dashboardData;
  } catch (error) {
    console.log('Error occurred in getdashboardData service : ', error);
    throw error;
  }
};

/**
 * Method to delete project
 * @param projectId
 */
const deleteProject = async (projectId: number) => {
  try {
    const projectExist = await projectDao.getById(projectId);
    if (!projectExist) {
      const result = {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await projectDao.deleteProject(projectId);
    if (data.is_delete === true) {
      const result = {
        message: 'Project Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this project',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteProject project service : ', error);
    throw error;
  }
};

/**
 * Method to search Project - Pagination API
 * @returns
 */
const searchProject = async (body) => {
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
    const project_manager_id = body.project_manager_id;
    const project_status = body.project_status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterProject = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (project_manager_id) {
      filterObj.filterProject = filterObj.filterProject || {};
      filterObj.filterProject.AND = filterObj.filterProject.AND || [];
      filterObj.filterProject.AND.push({
        user_id: project_manager_id,
      });
    }

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

    const result = await projectDao.searchProject(
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
    console.log('Error occurred in searchProject Project service : ', error);
    throw error;
  }
};

/**
 * Method to get Project By code
 * @param body
 * @returns
 */
const getByCode = async (body) => {
  try {
    const { code } = body;
    let result = null;
    const projectData = await projectDao.getByCode(code);
    if (projectData) {
      result = {
        message: 'This code already exist',
        status: true,
        is_exist: true,
        data: projectData,
      };
      return result;
    } else {
      result = {
        message: 'This code does not exist',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByCode project service : ', error);
    throw error;
  }
};

/**
 * Method to Get Project Site Estimation
 * @returns
 */
const getByProjectIdAndSiteId = async (body) => {
  try {
    const { project_id, site_id } = body;
    const result = await projectSiteDao.getByProjectIdAndSiteId(
      project_id,
      site_id
    );
    if (result) {
      const projectData = { message: 'success', status: true, data: result };
      return projectData;
    } else {
      const projectData = {
        message: 'project_id and site_id combination does not exist',
        status: false,
        data: result,
      };
      return projectData;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectIdAndSiteId project service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get Project By user_id
 * @param user_id
 * @returns
 */
const getByUserId = async (user_id: number) => {
  try {
    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        return {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
      }

      const projectData = await projectDao.getByUserId(user_id);
      if (projectData.length > 0) {
        return { message: 'success', status: true, data: projectData };
      } else {
        return {
          message: 'No data found for this user_id',
          status: false,
          data: null,
        };
      }
    } else {
      const projectData = await projectDao.getAll();
      if (projectData.length > 0) {
        return { message: 'success', status: true, data: projectData };
      } else {
        return {
          message: 'No data found',
          status: false,
          data: null,
        };
      }
    }
  } catch (error) {
    console.log('Error occurred in getByUserId project service : ', error);
    throw error;
  }
};

export {
  createProject,
  updateProject,
  getAllProject,
  getById,
  deleteProject,
  searchProject,
  getByCode,
  getByProjectIdAndSiteId,
  getAllDashboard,
  getByUserId,
};
