import prisma from '../utils/prisma';

const add = async (
  center_id: BigInteger,
  user_name: string,
  user_password: string,
  mobile_number: string,
  email_id: string,
  first_name: string,
  last_name: string,
  profile_img_url: string,
  gender: string,
  dob: Date,
  status: string,
  address: string,
  created_by: BigInteger,
  updated_by: BigInteger,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const user = await transaction.users.create({
      data: {
        center_id: Number(center_id),
        user_name,
        user_password,
        mobile_number,
        email_id,
        first_name,
        last_name,
        profile_img_url,
        gender,
        dob,
        status,
        address,
        created_by: created_by ? Number(created_by) : null,
        created_date: currentDate,
        updated_by: updated_by ? Number(updated_by) : null,
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
  } catch (error) {
    console.log('Error occurred in user getByEmailId dao', error);
    throw error;
  }
};

const getByUserName = async (userName: string) => {
  try {
    const users = await prisma.users.findUnique({
      where: {
        user_name: userName,
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
    console.log('Error occurred in user getByUserName dao', error);
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
  getByUserName,
  getAllUserData,
};
