import prisma from '../utils/prisma';

const add = async (
  role_id: BigInteger,
  user_id: BigInteger,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userRole = await transaction.user_role.create({
      data: {
        role_id,
        user_id,
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
