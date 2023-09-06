import prisma from '../utils/prisma';

const add = async (
  user_password: string,
  contact_no: string,
  email_id: string,
  first_name: string,
  last_name: string,
  user_status: string,
  created_by: bigint,
  department: string,
  parent_user_id: number,
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
        created_by,
        is_delete,
        created_date: currentDate,
        updated_date: currentDate,
        department,
        is_initial_login: isInitialLogin,
        parent_user_id,
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
  updated_by: bigint,
  user_id: number,
  department: string,
  is_two_factor: boolean,
  parent_user_id: number,
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
        updated_by,
        updated_date: currentDate,
        department,
        is_two_factor,
        parent_user_id,
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
    const user = await prisma.users.findFirst({
      where: {
        user_id: Number(userId),
        is_delete: false,
      },
      // include: {
      //   parent_data: true,
      // },
    });

    return user;
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
        select: {
          user_id: true,
          first_name: true,
          last_name: true,
          user_password: true,
          contact_no: true,
          email_id: true,
          user_status: true,
          created_by: true,
          created_date: true,
          updated_by: true,
          updated_date: true,
          is_delete: true,
          is_initial_login: true,
          department: true,
          is_two_factor: true,
          parent_user_id: true,
          user_roles: {
            select: {
              role_data: {
                select: {
                  role_name: true,
                },
              },
            },
          },
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
      // include: {
      //   parent_data: true,
      // },
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
    const currentDate = new Date();
    const user = await prisma.users.update({
      where: {
        user_id: Number(userId),
      },
      data: {
        is_delete: true,
        user_status: 'IN',
        updated_date: currentDate,
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
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterUser;
    const users = await transaction.users.findMany({
      where: filter,
      select: {
        user_id: true,
        first_name: true,
        last_name: true,
        email_id: true,
        contact_no: true,
        address: true,
        created_date: true,
        updated_date: true,
        created_by: true,
        updated_by: true,
        department: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const usersCount = await transaction.users.count({
      where: filter,
    });
    const userData = {
      count: usersCount,
      data: users,
    };
    return userData;
  } catch (error) {
    console.log('Error occurred in user dao : customFilterUser ', error);
    throw error;
  }
};

const getByUniqueEmail = async (emailId: string) => {
  try {
    if (emailId) {
      const lowercasedEmailId = emailId.toLowerCase();
      const user = await prisma.users.findFirst({
        where: {
          email_id: lowercasedEmailId,
        },
      });
      return user;
    }
  } catch (error) {
    console.log('Error occurred in user getByUniqueEmail dao', error);
    throw error;
  }
};

const getAllSalesPersonUsers = async () => {
  try {
    const user = await prisma.users.findMany({
      where: {
        user_status: 'AC',
        is_delete: false,
      },
    });
    return user;
  } catch (error) {
    console.log('Error occurred in user getAllSalesPersonUsers dao', error);
    throw error;
  }
};

const searchUser = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterUser;
    const user = await transaction.users.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
      include: {
        parent_data: true,
      },
    });
    const userCount = await transaction.users.count({
      where: filter,
    });

    const userData = {
      count: userCount,
      data: user,
    };
    return userData;
  } catch (error) {
    console.log('Error occurred in user dao : searchUser ', error);
    throw error;
  }
};

const getUserByRoleName = async (role_name: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const users = await transaction.users.findMany({
      where: {
        user_status: 'AC',
        is_delete: false,
        user_roles: {
          some: {
            role_data: {
              role_name: role_name,
            },
          },
        },
      },
      include: {
        user_roles: {
          include: {
            role_data: {
              select: { role_name: true },
            },
          },
        },
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return users;
  } catch (error) {
    console.log('Error occurred in user getUserByRoleName dao', error);
    throw error;
  }
};

const getChildUsersByParentUserId = async (
  parent_user_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const users = await transaction.users.findMany({
      where: {
        user_status: 'AC',
        is_delete: false,
        parent_user_id: parent_user_id,
      },
      include: {
        user_roles: {
          include: {
            role_data: {
              select: { role_name: true },
            },
          },
        },
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return users;
  } catch (error) {
    console.log(
      'Error occurred in user getChildUsersByParentUserId dao',
      error
    );
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
  getByUniqueEmail,
  getAllSalesPersonUsers,
  searchUser,
  getUserByRoleName,
  getChildUsersByParentUserId,
};
