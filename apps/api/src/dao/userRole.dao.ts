import prisma from '../utils/prisma';

const add = async (
  role_id: BigInteger,
  user_id: BigInteger,
  created_by: BigInteger,
  updated_by: BigInteger,
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
        updated_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });

    const modifiedUserRole = {
      ...userRole,
      user_role_id: Number(userRole.user_role_id),
    };
    return modifiedUserRole;
  } catch (error) {
    console.log('Error occurred in userRoleDao add', error);
    throw error;
  }
};

export default { add };
