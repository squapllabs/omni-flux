import prisma from '../utils/prisma';

const editPassword = async (email_id, user_password, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_initial_login = false;
    const currentDate = new Date();
    const result = await transaction.users.update({
      where: {
        email_id: String(email_id),
      },
      data: {
        user_password: user_password,
        is_initial_login: is_initial_login,
        updated_date: currentDate,
      },
    });
    const updatedData = {
      ...result,
      user_id: Number(result.user_id),
    };
    return updatedData;
  } catch (error) {
    console.log('Error occurred in editPassword dao', error);
    throw error;
  }
};

export default {
  editPassword,
};
