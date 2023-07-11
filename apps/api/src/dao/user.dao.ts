import prisma from '../utils/prisma';

const add = async (
  user_password: string,
  contact_no: string,
  email_id: string,
  first_name: string,
  last_name: string,
  user_status: string,
  address: string,
  created_by: BigInteger,
  updated_by: BigInteger,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const user = await transaction.users.create({
      data: {
        user_password,
        contact_no,
        email_id,
        first_name,
        last_name,
        user_status,
        address,
        created_by,
        updated_by,
        is_delete,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });

    const modifiedUser = {
      ...user,
      user_id: Number(user.user_id),
    };
    return modifiedUser;
  } catch (error) {
    console.log('Error occurred in userDao add dao', error);
    throw error;
  }
};

const getById = async (userId: bigint) => {
  try {
    const users = await prisma.users.findUnique({
      where: {
        user_id: Number(userId),
      },
    });

    if (users) {
      const modifiedUsers = {
        ...users,
        user_id: Number(users.user_id),
      };
      return modifiedUsers;
    } else {
      return users;
    }
  } catch (error) {
    console.log('Error occurred in user getById dao', error);
    throw error;
  }
};

const getByEmailId = async (emailId: string) => {
  try {
    if (emailId) {
      const users = await prisma.users.findFirst({
        where: {
          email_id: emailId,
        },
      });

      if (users) {
        const modifiedUsers = {
          ...users,
          user_id: Number(users.user_id),
        };
        return modifiedUsers;
      } else {
        return users;
      }
    }
  } catch (error) {
    console.log('Error occurred in user getByEmailId dao', error);
    throw error;
  }
};

const getAllUserData = async () => {
  try {
    const users = await prisma.users.findMany({});
    const modifiedUsers = users.map((user) => ({
      ...user,
      user_id: Number(user.user_id),
    }));

    return modifiedUsers;
  } catch (error) {
    console.log('Error occurred in user getAllUserData dao', error);
    throw error;
  }
};

export default {
  add,
  getById,
  getByEmailId,
  getAllUserData,
};
