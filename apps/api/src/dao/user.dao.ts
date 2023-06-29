import db from '../utils/db';
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
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `INSERT INTO users(center_id,user_name,user_password,mobile_number,
        email_id,first_name,last_name,profile_img_url,gender,dob,status,address,created_by,
        created_date,updated_by,updated_date) 
          values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16) RETURNING *`;
    const result = await transaction.one(query, [
      center_id,
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
      created_by,
      currentDate,
      updated_by,
      currentDate,
    ]);

    /*   const result = await prisma.$transaction(async (prisma) => {
      // const transaction = connectionObj !== null ? connectionObj : prisma;

      const transaction = connectionObj ?? prisma;
      console.log('transaction in dao', transaction);

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
      console.log('user', user);

      const modifiedUser = {
        ...user,
        user_id: Number(user.user_id),
      };

      return modifiedUser;
    }); */

    return result;
  } catch (error) {
    console.log('Error occurred in userDao add dao', error);
    throw error;
  }
};

const getById = async (userId: bigint) => {
  try {
    /*  let result = null;
    const query = `select * from users where user_id=$1`;
    result = await db.oneOrNone(query, [userId]);
    return result; */
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
    console.log('Error occurred in getById dao', error);
    throw error;
  }
};

const getByEmailId = async (emailId: string) => {
  try {
    /* let result = null;
    const query = `select * from users where email_id=$1`;
    result = await db.oneOrNone(query, [emailId]);
    return result; */

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
    console.log('Error occurred in getByEmailId dao', error);
    throw error;
  }
};

const getByUserName = async (user_name: string) => {
  try {
    let result = null;
    const query = `select * from users where user_name=$1`;
    result = await db.oneOrNone(query, [user_name]);
    return result;
  } catch (error) {
    console.log('Error occurred in getByUserName dao', error);
    throw error;
  }
};

const userLogin = async (email: string, userpass: string) => {
  try {
    let result = null;
    const query = `select * from users where email=$1 and userpass=$2`;
    result = await db.oneOrNone(query, [email, userpass]);
    return result;
  } catch (error) {
    console.log('Error occurred in userLogin dao', error);
    throw error;
  }
};

/* const getAllUserData = async () => {
  try {
    const query = `select * from users`;
    const result = await db.manyOrNone(query, []);
    return result;
  } catch (error) {
    console.log('Error occurred in getAllUserData', error);
    throw error;
  }
}; */

const getAllUserData = async () => {
  try {
    const users = await prisma.users.findMany({});
    const modifiedUsers = users.map((user) => ({
      ...user,
      user_id: Number(user.user_id),
    }));

    return modifiedUsers;
  } catch (error) {
    console.log('Error occurred in getAllUserData dao', error);
    throw error;
  }
};

export default {
  add,
  getById,
  getByEmailId,
  userLogin,
  getByUserName,
  getAllUserData,
};
