import prisma from '../utils/prisma';

const getAll = async (user_status) => {
  try {
    const users = await prisma.users.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
      where: {
        user_status: user_status,
      },
    });
    const usersCount = await prisma.users.count({
      where: {
        user_status: user_status,
      },
    });
    const userData = {
      count: usersCount,
      data: users,
    };
    return userData;
  } catch (error) {
    console.log('Error occurred in user getAll dao', error);
    throw error;
  }
};

export default {
  getAll,
};
