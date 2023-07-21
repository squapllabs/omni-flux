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
  department: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const isInitialLogin = true;
    const lowercasedEmailId = email_id.toLowerCase();
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const user = await transaction.users.create({
      data: {
        user_password,
        contact_no,
        email_id: lowercasedEmailId,
        first_name,
        last_name,
        user_status,
        address,
        created_by,
        is_delete,
        created_date: currentDate,
        updated_date: currentDate,
        department,
        is_initial_login: isInitialLogin,
      },
    });
    return user;
  } catch (error) {
    console.log('Error occurred in userDao add dao', error);
    throw error;
  }
};

const edit = async (
  first_name: string,
  last_name: string,
  address: string,
  updated_by: bigint,
  user_id: number,
  department: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const user = await transaction.users.update({
      where: { user_id },
      data: {
        first_name,
        last_name,
        address,
        updated_by,
        updated_date: currentDate,
        department,
      },
    });
    return user;
  } catch (error) {
    console.log('Error occurred in userDao edit dao', error);
    throw error;
  }
};

const getById = async (userId: number) => {
  try {
    const user = await prisma.users.findUnique({
      where: {
        user_id: Number(userId),
      },
    });
    if (user && user?.is_delete === true) {
      return null;
    } else {
      return user;
    }
  } catch (error) {
    console.log('Error occurred in user getById dao', error);
    throw error;
  }
};

const getByEmailId = async (emailId: string) => {
  try {
    if (emailId) {
      const lowercasedEmailId = emailId.toLowerCase();
      const user = await prisma.users.findFirst({
        where: {
          email_id: lowercasedEmailId,
          user_status: 'AC',
          is_delete: false,
        },
      });
      return user;
    }
  } catch (error) {
    console.log('Error occurred in user getByEmailId dao', error);
    throw error;
  }
};

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

const getUserDataWithRoleId = async (userId: number) => {
  try {
    const user = await prisma.$queryRaw`
      SELECT *
      FROM users u
      LEFT JOIN user_roles ur ON u.user_id = ur.user_id
      WHERE u.user_id = ${userId}`;

    return user;
  } catch (error) {
    console.log('Error occurred in user getUserDataWithRoleId dao', error);
    throw error;
  }
};

const updateStatus = async (
  user_id: number,
  user_status: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const user = await transaction.users.update({
      where: { user_id },
      data: {
        user_status,
        updated_date: currentDate,
      },
    });
    return user;
  } catch (error) {
    console.log('Error occurred in userDao updateStatus dao', error);
    throw error;
  }
};

const getDeletedUsers = async () => {
  try {
    const users = await prisma.users.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
      where: {
        is_delete: true,
      },
    });
    const usersCount = await prisma.users.count({
      where: {
        is_delete: true,
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

const customFilterUser = async (
  offset,
  limit,
  orderByColumn,
  orderByDirection,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.where;
    const users = await transaction.item.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
      // include: {
      //   gst: {
      //     select: {
      //       rate: true,
      //       gst_id: true,
      //     },
      //   },
      //   hsn_code: {
      //     select: {
      //       code: true,
      //       hsn_code_id: true,
      //     },
      //   },
      //   uom: {
      //     select: {
      //       name: true,
      //       uom_id: true,
      //     },
      //   },

      //   sub_sub_category: {
      //     select: {
      //       name: true,
      //       sub_sub_category_id: true,
      //     },
      //   },

      //   item_type: {
      //     select: {
      //       item_type_item_name: true,
      //     },
      //   },

      //   brand: {
      //     select: {
      //       brand_name: true,
      //     },
      //   },
      // },
    });
    return users;
  } catch (error) {
    console.log('Error occurred in user dao : customFilterUser ', error);
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
  updateStatus,
  getDeletedUsers,
  customFilterUser,
};
