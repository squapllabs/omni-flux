import prisma from '../utils/prisma';

const add = async (
  role_id: bigint,
  user_id: bigint,
  created_by: bigint,
  updated_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const dbUserRole = await transaction.user_roles.create({
      data: {
        role_id,
        user_id,
        created_by,
        updated_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });

    const userRole = {
      ...dbUserRole,
      user_role_id: Number(dbUserRole.user_role_id),
    };
    return userRole;
  } catch (error) {
    console.log('Error occurred in userRoleDao add', error);
    throw error;
  }
};

const edit = async (
  role_id: bigint,
  user_id: bigint,
  created_by: bigint,
  updated_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const dbUserRole = await transaction.user_roles.update({
      where: {
        uk_user_roles_user_id_role_id: { role_id: role_id, user_id: user_id },
      },
      data: {
        role_id,
        user_id,
        created_by,
        updated_by,
        updated_date: currentDate,
      },
    });

    const userRole = {
      ...dbUserRole,
      user_role_id: Number(dbUserRole.user_role_id),
    };
    return userRole;
  } catch (error) {
    console.log('Error occurred in userRoleDao edit', error);
    throw error;
  }
};

export default { add, edit };
