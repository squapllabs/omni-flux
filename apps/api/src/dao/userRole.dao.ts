import prisma from '../utils/prisma';

const add = async (
  role_id: bigint,
  user_id: bigint,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userRole = await transaction.user_roles.create({
      data: {
        role_id,
        user_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return userRole;
  } catch (error) {
    console.log('Error occurred in userRoleDao add', error);
    throw error;
  }
};

const edit = async (
  role_id: bigint,
  user_id: bigint,
  updated_by: bigint,
  user_role_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userRole = await transaction.user_roles.update({
      where: {
        user_role_id: user_role_id,
      },
      data: {
        role_id,
        user_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return userRole;
  } catch (error) {
    console.log('Error occurred in userRoleDao edit', error);
    throw error;
  }
};

const getById = async (userRoleId: number) => {
  try {
    const userRole = await prisma.user_roles.findUnique({
      where: {
        user_role_id: Number(userRoleId),
      },
    });
    return userRole;
  } catch (error) {
    console.log('Error occurred in userRole getById dao', error);
    throw error;
  }
};

const getByUserId = async (userId: number) => {
  try {
    if (userId) {
      const userRole = await prisma.user_roles.findFirst({
        where: {
          user_id: Number(userId),
        },
      });
      return userRole;
    }
  } catch (error) {
    console.log('Error occurred in userRole getByEmailId dao', error);
    throw error;
  }
};

const getAll = async () => {
  try {
    const userRole = await prisma.user_roles.findMany({});
    return userRole;
  } catch (error) {
    console.log('Error occurred in userRole getAll dao', error);
    throw error;
  }
};

const deleteUserRole = async (userRoleId: number) => {
  try {
    const userRole = await prisma.user_roles.delete({
      where: {
        user_role_id: Number(userRoleId),
      },
    });
    return userRole;
  } catch (error) {
    console.log('Error occurred in userRole deleteUserRole dao', error);
    throw error;
  }
};

export default { add, edit, getById, getByUserId, getAll, deleteUserRole };
