import prisma from '../utils/prisma';

const add = async (
  user_id: number,
  project_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userPrimaryProject = await transaction.user_primary_project.create({
      data: {
        user_id,
        project_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return userPrimaryProject;
  } catch (error) {
    console.log('Error occurred in userPrimaryProjectDao add', error);
    throw error;
  }
};

const edit = async (
  user_id: number,
  project_id: number,
  updated_by: number,
  user_primary_project_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userPrimaryProject = await transaction.user_primary_project.update({
      where: {
        user_primary_project_id: user_primary_project_id,
      },
      data: {
        user_id,
        project_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return userPrimaryProject;
  } catch (error) {
    console.log('Error occurred in userPrimaryProjectDao edit', error);
    throw error;
  }
};

const getById = async (userPrimaryProjectId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userPrimaryProject =
      await transaction.user_primary_project.findUnique({
        where: {
          user_primary_project_id: Number(userPrimaryProjectId),
        },
        include: {
          user_data: { select: { first_name: true, last_name: true } },
          project_data: true,
        },
      });
    return userPrimaryProject;
  } catch (error) {
    console.log('Error occurred in userPrimaryProject getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userPrimaryProject = await transaction.user_primary_project.findMany({
      include: {
        user_data: { select: { first_name: true, last_name: true } },
        project_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return userPrimaryProject;
  } catch (error) {
    console.log('Error occurred in userPrimaryProject getAll dao', error);
    throw error;
  }
};

const deleteUserPrimaryProject = async (
  userPrimaryProjectId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userPrimaryProject = await transaction.user_primary_project.delete({
      where: {
        user_primary_project_id: Number(userPrimaryProjectId),
      },
    });
    return userPrimaryProject;
  } catch (error) {
    console.log(
      'Error occurred in userPrimaryProject deleteUserPrimaryProject dao',
      error
    );
    throw error;
  }
};

const getByUserId = async (user_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userPrimaryProject = await transaction.user_primary_project.findFirst(
      {
        where: {
          user_id: Number(user_id),
        },
        include: {
          user_data: { select: { first_name: true, last_name: true } },
          project_data: true,
        },
      }
    );
    return userPrimaryProject;
  } catch (error) {
    console.log('Error occurred in userPrimaryProject getByUserId dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteUserPrimaryProject,
  getByUserId,
};
