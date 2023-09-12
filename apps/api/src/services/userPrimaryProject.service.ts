import projectDao from '../dao/project.dao';
import userDao from '../dao/user.dao';
import userPrimaryProjectDao from '../dao/userPrimaryProject.dao';
import { userPrimaryProjectBody } from '../interfaces/userPrimaryProject.interface';

/**
 * Method to Create a New UserPrimaryProject
 * @param body
 * @returns
 */
const createUserPrimaryProject = async (body: userPrimaryProjectBody) => {
  try {
    const { user_id, project_id, created_by } = body;

    if (user_id) {
      const userExist = await userDao.getById(user_id);

      const userPrimaryProjectExistForThisUser =
        await userPrimaryProjectDao.getByUserId(user_id);

      if (!userExist) {
        return {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
      }

      if (userPrimaryProjectExistForThisUser) {
        return {
          message: 'user_id already exist in user_primary_project',
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

    const userPrimaryProjectDetails = await userPrimaryProjectDao.add(
      user_id,
      project_id,
      created_by
    );
    const result = {
      message: 'success',
      status: true,
      data: userPrimaryProjectDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in userPrimaryProject service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing UserPrimaryProject
 * @param body
 * @returns
 */
const updateUserPrimaryProject = async (body: userPrimaryProjectBody) => {
  try {
    const { user_id, project_id, updated_by, user_primary_project_id } = body;
    let result = null;
    const userPrimaryProjectExist = await userPrimaryProjectDao.getById(
      user_primary_project_id
    );

    if (!userPrimaryProjectExist) {
      return {
        message: 'user_primary_project_id does not exist',
        status: false,
        data: null,
      };
    }

    if (user_id) {
      const userExist = await userDao.getById(user_id);

      const userPrimaryProjectExistForThisUser =
        await userPrimaryProjectDao.getByUserId(user_id);

      if (!userExist) {
        return {
          message: 'user_id does not exist',
          status: false,
          data: null,
        };
      }

      if (
        userPrimaryProjectExistForThisUser &&
        userPrimaryProjectExistForThisUser?.user_primary_project_id !==
          user_primary_project_id
      ) {
        return {
          message: 'user_id already exist in user_primary_project',
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

    const userPrimaryProjectDetails = await userPrimaryProjectDao.edit(
      user_id,
      project_id,
      updated_by,
      user_primary_project_id
    );
    result = {
      message: 'success',
      status: true,
      data: userPrimaryProjectDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in userPrimaryProject service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get UserPrimaryProject By UserPrimaryProjectId
 * @param userPrimaryProjectId
 * @returns
 */
const getById = async (userPrimaryProjectId: number) => {
  try {
    let result = null;
    const userPrimaryProjectData = await userPrimaryProjectDao.getById(
      userPrimaryProjectId
    );
    if (userPrimaryProjectData) {
      result = {
        message: 'success',
        status: true,
        data: userPrimaryProjectData,
      };
      return result;
    } else {
      result = {
        message: 'user_primary_project_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getById userPrimaryProject service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All UserPrimaryProject's
 * @returns
 */
const getAllUserPrimaryProject = async () => {
  try {
    const result = await userPrimaryProjectDao.getAll();
    const userPrimaryProjectData = {
      message: 'success',
      status: true,
      data: result,
    };
    return userPrimaryProjectData;
  } catch (error) {
    console.log(
      'Error occurred in getAllUserPrimaryProject userPrimaryProject service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete userPrimaryProject
 * @param userPrimaryProjectId
 */
const deleteUserPrimaryProject = async (userPrimaryProjectId: number) => {
  try {
    const userPrimaryProjectExist = await userPrimaryProjectDao.getById(
      userPrimaryProjectId
    );
    if (!userPrimaryProjectExist) {
      const result = {
        message: 'user_primary_project_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await userPrimaryProjectDao.deleteUserPrimaryProject(
      userPrimaryProjectId
    );
    if (data) {
      const result = {
        message: 'UserPrimaryProject Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this userPrimaryProject',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteUserPrimaryProject userPrimaryProject service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get UserPrimaryProject By user_id
 * @param user_id
 * @returns
 */
const getByUserId = async (user_id: number) => {
  try {
    let result = null;

    const userExist = await userDao.getById(user_id);
    if (!userExist) {
      return {
        message: 'user_id does not exist',
        status: false,
        data: null,
      };
    }

    const userPrimaryProjectData = await userPrimaryProjectDao.getByUserId(
      user_id
    );
    if (userPrimaryProjectData) {
      result = {
        message: 'user_id already exist in user_primary_project',
        status: true,
        is_exist: true,
        data: userPrimaryProjectData,
      };
      return result;
    } else {
      result = {
        message: 'user_id does not exist in user_primary_project',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByUserId userPrimaryProject service : ',
      error
    );
    throw error;
  }
};

export {
  createUserPrimaryProject,
  updateUserPrimaryProject,
  getAllUserPrimaryProject,
  getById,
  deleteUserPrimaryProject,
  getByUserId,
};
