import prisma from '../utils/prisma';

const add = async (
  role_name: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const role = await transaction.role.create({
      data: {
        role_name,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return role;
  } catch (error) {
    console.log('Error occurred in roleDao add', error);
    throw error;
  }
};

const edit = async (
  role_name: string,
  updated_by: bigint,
  role_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const role = await transaction.role.update({
      where: {
        role_id: role_id,
      },
      data: {
        role_name,
        updated_by,
        updated_date: currentDate,
      },
    });
    return role;
  } catch (error) {
    console.log('Error occurred in roleDao edit', error);
    throw error;
  }
};

const getById = async (roleId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const role = await transaction.role.findUnique({
      where: {
        role_id: Number(roleId),
      },
    });
    return role;
  } catch (error) {
    console.log('Error occurred in role getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const role = await transaction.role.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return role;
  } catch (error) {
    console.log('Error occurred in role getAll dao', error);
    throw error;
  }
};

const deleteRole = async (roleId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const role = await transaction.role.delete({
      where: {
        role_id: Number(roleId),
      },
    });
    return role;
  } catch (error) {
    console.log('Error occurred in role deleteRole dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteRole,
};
