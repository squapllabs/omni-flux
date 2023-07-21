import projectDao from '../dao/project.dao';
import userDao from '../dao/user.dao';
import clientDao from '../dao/client.dao';
import {
  createProjectBody,
  updateProjectBody,
} from '../interfaces/project.Interface';
// import customQueryExecutor from '../dao/common/utils.dao';
import prisma from '../utils/prisma';

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
      budget,
      client_id,
      document_url,
      created_by,
    } = body;
    let result = null;

    const userExist = await userDao.getById(user_id);
    if (!userExist) {
      result = { success: false, message: 'user_id does not exist' };
      return result;
    }

    const clientExist = await clientDao.getById(client_id);
    if (!clientExist) {
      result = { success: false, message: 'client_id does not exist' };
      return result;
    }

    const projectDetails = await projectDao.add(
      project_name,
      description,
      user_id,
      date_started,
      date_ended,
      status,
      budget,
      client_id,
      document_url,
      created_by
    );
    result = { success: true, data: projectDetails };
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
      budget,
      client_id,
      document_url,
      updated_by,
      project_id,
    } = body;
    let result = null;

    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        result = { success: false, message: 'user_id does not exist' };
        return result;
      }
    }
    if (client_id) {
      const clientExist = await clientDao.getById(client_id);
      if (!clientExist) {
        result = { success: false, message: 'client_id does not exist' };
        return result;
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
        budget,
        client_id,
        document_url,
        updated_by,
        project_id
      );
      result = { success: true, data: projectDetails };
      return result;
    } else {
      result = { success: false, message: 'project_id does not exist' };
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
      result = { success: true, data: projectData };
      return result;
    } else {
      result = { success: false, message: 'project_id does not exist' };
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
    const projectData = { success: true, data: result };
    return projectData;
  } catch (error) {
    console.log('Error occurred in getAllProject project service : ', error);
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
      const result = { success: false, message: 'project_id does not exist' };
      return result;
    }
    const data = await projectDao.deleteProject(projectId);
    if (data.is_delete === true) {
      const result = {
        success: true,
        message: 'Project Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this project',
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteProject project service : ', error);
    throw error;
  }
};

/**
 * Method for custom filter API
 * @param body
 * @returns
 */
const customFilterProject = async (body) => {
  try {
    const {
      size = 10,
      page = 0,
      sort = 'desc',
      project_name,
      user_name,
      client_name,
    } = body;

    const skip = page > 0 ? page * size : 0;

    const projects = await prisma.project.findMany({
      where: {
        is_delete: false,
        project_name: { contains: project_name || '' },
        user: {
          AND: [
            { first_name: { contains: user_name || '' } },
            { last_name: { contains: user_name || '' } },
          ],
        },
        client: { name: { contains: client_name || '' } },
      },
      include: {
        user: { select: { first_name: true, last_name: true } },
        client: { select: { name: true } },
      },
      orderBy: { updated_date: sort },
      take: size,
      skip: skip,
    });

    const totalCount = await prisma.project.count({
      where: {
        is_delete: false,
        project_name: { contains: project_name || '' },
        user: {
          AND: [
            { first_name: { contains: user_name || '' } },
            { last_name: { contains: user_name || '' } },
          ],
        },
        client: { name: { contains: client_name || '' } },
      },
    });

    const totalPages = Math.ceil(totalCount / size);

    const projectData = {
      total_count: totalCount,
      total_page: totalPages,
      size: size,
      content: projects,
    };

    const result = {
      message: 'success',
      status: true,
      data: projectData,
    };

    return result;
  } catch (error) {
    console.log(
      'Error occurred in customFilterProject project service:',
      error
    );
    throw error;
  }
};

export {
  createProject,
  updateProject,
  getAllProject,
  getById,
  deleteProject,
  customFilterProject,
};
