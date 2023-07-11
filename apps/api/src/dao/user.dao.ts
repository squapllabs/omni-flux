import prisma from '../utils/prisma';

const add = async (
  user_password: string,
  contact_no: string,
  email_id: string,
  first_name: string,
  last_name: string,
  user_status: string,
  address: string,
  created_by: bigint,
  updated_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const dbUser = await transaction.users.create({
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

    const user = {
      ...dbUser,
      user_id: Number(dbUser.user_id),
    };
    return user;
  } catch (error) {
    console.log('Error occurred in userDao add dao', error);
    throw error;
  }
};

const edit = async (
  user_password: string,
  contact_no: string,
  email_id: string,
  first_name: string,
  last_name: string,
  user_status: string,
  address: string,
  created_by: bigint,
  updated_by: bigint,
  user_id: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const dbUser = await transaction.users.update({
      where: { user_id },
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
        updated_date: currentDate,
      },
    });

    const user = {
      ...dbUser,
      user_id: Number(dbUser.user_id),
    };
    return user;
  } catch (error) {
    console.log('Error occurred in userDao edit dao', error);
    throw error;
  }
};

const getById = async (userId: bigint) => {
  try {
    const dbUser = await prisma.users.findUnique({
      where: {
        user_id: Number(userId),
      },
    });

    if (dbUser) {
      const user = {
        ...dbUser,
        user_id: Number(dbUser.user_id),
      };
      return user;
    } else {
      return dbUser;
    }
  } catch (error) {
    console.log('Error occurred in user getById dao', error);
    throw error;
  }
};

const getByEmailId = async (emailId: string) => {
  try {
    if (emailId) {
      const dbUser = await prisma.users.findFirst({
        where: {
          email_id: emailId,
          user_status: 'AC',
          is_delete: false,
        },
      });

      if (dbUser) {
        const user = {
          ...dbUser,
          user_id: Number(dbUser.user_id),
        };
        return user;
      } else {
        return dbUser;
      }
    }
  } catch (error) {
    console.log('Error occurred in user getByEmailId dao', error);
    throw error;
  }
};

const getAll = async (user_status) => {
  try {
    const dbUser = await prisma.users.findMany({
      where: {
        user_status: user_status,
      },
    });
    const users = dbUser.map((user) => ({
      ...user,
      user_id: Number(user.user_id),
    }));

    return users;
  } catch (error) {
    console.log('Error occurred in user getAll dao', error);
    throw error;
  }
};

const deleteUser = async (userId: bigint) => {
  try {
    const user = await prisma.users.update({
      where: {
        user_id: Number(userId),
      },
      data: {
        is_delete: true,
        user_status: 'IN',
      },
    });

    return user;
  } catch (error) {
    console.log('Error occurred in user deleteUser dao', error);
    throw error;
  }
};

const getUserDataWithRoleId = async (userId: bigint) => {
  try {
    const user = await prisma.$queryRaw`
      SELECT *
      FROM users u
      LEFT JOIN user_roles ur ON u.user_id = ur.user_id
      WHERE u.user_id = ${Number(userId)}`;

    return user;
  } catch (error) {
    console.log('Error occurred in user getUserDataWithRoleId dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getByEmailId,
  getAll,
  deleteUser,
  getUserDataWithRoleId,
};
