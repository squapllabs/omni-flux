import prisma from '../utils/prisma';

const add = async (
  user_id: number,
  profile_image_url: string,
  date_of_birth: Date,
  gender: string,
  address: JSON,
  additional_info: JSON,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formattedDOB = date_of_birth ? new Date(date_of_birth) : null;
    const userProfile = await transaction.user_profiles.create({
      data: {
        user_id,
        profile_image_url,
        date_of_birth: formattedDOB,
        gender,
        address,
        additional_info,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return userProfile;
  } catch (error) {
    console.log('Error occurred in userProfileDao add', error);
    throw error;
  }
};

const edit = async (
  profile_image_url: string,
  date_of_birth: Date,
  gender: string,
  address: JSON,
  additional_info: JSON,
  updated_by: bigint,
  user_profile_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formattedDOB = date_of_birth ? new Date(date_of_birth) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userProfile = await transaction.user_profiles.update({
      where: {
        user_profile_id: user_profile_id,
      },
      data: {
        profile_image_url,
        date_of_birth: formattedDOB,
        updated_by,
        gender,
        address,
        additional_info,
        updated_date: currentDate,
      },
    });
    return userProfile;
  } catch (error) {
    console.log('Error occurred in userProfileDao edit', error);
    throw error;
  }
};

const getById = async (userProfileId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userProfile = await transaction.user_profiles.findUnique({
      where: {
        user_profile_id: Number(userProfileId),
        is_delete: false,
      },
    });
    return userProfile;
  } catch (error) {
    console.log('Error occurred in userProfile getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userProfile = await transaction.user_profiles.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return userProfile;
  } catch (error) {
    console.log('Error occurred in userProfile getAll dao', error);
    throw error;
  }
};

const deleteUserProfile = async (
  userProfileId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userProfile = await transaction.user_profiles.update({
      where: {
        user_profile_id: Number(userProfileId),
      },
      data: {
        is_delete: true,
      },
    });
    return userProfile;
  } catch (error) {
    console.log('Error occurred in userProfile deleteUserProfile dao', error);
    throw error;
  }
};

const getByUserId = async (userId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const userProfile = await transaction.user_profiles.findFirst({
      where: {
        user_id: Number(userId),
        is_delete: false,
      },
    });
    return userProfile;
  } catch (error) {
    console.log('Error occurred in userProfile getByUserId dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteUserProfile,
  getByUserId,
};
