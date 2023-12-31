import prisma from '../utils/prisma';

const add = async (
  project_id: number,
  user_id: number,
  project_role_id: number,
  access_start_date: Date,
  access_end_date: Date,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const formattedAccessStartDate = access_start_date
      ? new Date(access_start_date)
      : null;
    const formattedAccessEndDate = access_end_date
      ? new Date(access_end_date)
      : null;
    const projectMemberAssociation =
      await transaction.project_member_association.create({
        data: {
          project_id,
          user_id,
          project_role_id,
          access_start_date: formattedAccessStartDate,
          access_end_date: formattedAccessEndDate,
          created_by,
          is_delete: is_delete,
          created_date: currentDate,
          updated_date: currentDate,
        },
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log('Error occurred in projectMemberAssociationDao add', error);
    throw error;
  }
};

const edit = async (
  project_id: number,
  user_id: number,
  project_role_id: number,
  access_start_date: Date,
  access_end_date: Date,
  updated_by: number,
  project_member_association_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formattedAccessStartDate = access_start_date
      ? new Date(access_start_date)
      : null;
    const formattedAccessEndDate = access_end_date
      ? new Date(access_end_date)
      : null;
    const projectMemberAssociation =
      await transaction.project_member_association.update({
        where: {
          project_member_association_id: Number(project_member_association_id),
        },
        data: {
          project_id,
          user_id,
          project_role_id,
          access_start_date: formattedAccessStartDate,
          access_end_date: formattedAccessEndDate,
          updated_by,
          updated_date: currentDate,
        },
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log('Error occurred in projectMemberAssociationDao edit', error);
    throw error;
  }
};

const getById = async (
  projectMemberAssociationId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectMemberAssociation =
      await transaction.project_member_association.findFirst({
        where: {
          project_member_association_id: Number(projectMemberAssociationId),
          is_delete: false,
        },
        include: {
          project_data: true,
          user_data: {
            select: {
              first_name: true,
              last_name: true,
              email_id: true,
              user_profiles: {
                select: {
                  profile_image_url: true,
                },
              },
            },
          },
          project_role_data: { select: { role_name: true } },
        },
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation getById dao',
      error
    );
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectMemberAssociation =
      await transaction.project_member_association.findMany({
        where: {
          is_delete: false,
        },
        include: {
          project_data: true,
          user_data: {
            select: {
              first_name: true,
              last_name: true,
              email_id: true,
              user_profiles: {
                select: {
                  profile_image_url: true,
                },
              },
            },
          },
          project_role_data: { select: { role_name: true } },
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log('Error occurred in projectMemberAssociation getAll dao', error);
    throw error;
  }
};

const deleteProjectMemberAssociation = async (
  projectMemberAssociationId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectMemberAssociation =
      await transaction.project_member_association.update({
        where: {
          project_member_association_id: Number(projectMemberAssociationId),
        },
        data: {
          is_delete: true,
        },
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation deleteProjectMemberAssociation dao',
      error
    );
    throw error;
  }
};

const getByProjectIdAndUserId = async (
  project_id: number,
  user_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectMemberAssociation =
      await transaction.project_member_association.findFirst({
        where: {
          project_id: Number(project_id),
          user_id: Number(user_id),
          is_delete: false,
        },
        include: {
          project_data: true,
          user_data: {
            select: {
              first_name: true,
              last_name: true,
              email_id: true,
              user_profiles: {
                select: {
                  profile_image_url: true,
                },
              },
            },
          },
          project_role_data: { select: { role_name: true } },
        },
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation getByProjectIdAndUserId dao',
      error
    );
    throw error;
  }
};

const getByProjectId = async (project_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectMemberAssociation =
      await transaction.project_member_association.findMany({
        where: {
          project_id: Number(project_id),
          is_delete: false,
        },
        include: {
          project_data: true,
          user_data: {
            select: {
              first_name: true,
              last_name: true,
              email_id: true,
              user_profiles: {
                select: {
                  profile_image_url: true,
                },
              },
            },
          },
          project_role_data: { select: { role_name: true } },
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation getByProjectId dao',
      error
    );
    throw error;
  }
};

const searchProjectMemberAssociation = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterProjectMemberAssociation;
    const approver_status = filters.approver_status;

    const projectMemberAssociation =
      await transaction.project_member_association.findMany({
        where: filter,
        include: {
          project_data: {
            include: {
              indent_request: {
                where: {
                  project_id: {
                    not: {
                      equals: null,
                    },
                  },
                  approver_status: approver_status,
                  is_delete: false,
                },
              },
              user: {
                select: {
                  first_name: true,
                  last_name: true,
                },
              },
              client: {
                select: {
                  name: true,
                },
              },
            },
          },
          user_data: {
            select: {
              first_name: true,
              last_name: true,
              email_id: true,
              user_profiles: {
                select: {
                  profile_image_url: true,
                },
              },
            },
          },
          project_role_data: { select: { role_name: true } },
        },

        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
    const projectMemberAssociationCount =
      await transaction.project_member_association.count({
        where: filter,
      });

    const projectMemberAssociationData = {
      count: projectMemberAssociationCount,
      data: projectMemberAssociation,
    };
    return projectMemberAssociationData;
  } catch (error) {
    console.log(
      'Error occurred in Project Member Association dao :  searchProjectMemberAssociation ',
      error
    );
    throw error;
  }
};

const getByProjectIdAndRoleType = async (
  project_id: number,
  role_name: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectMemberAssociation =
      await transaction.project_member_association.findMany({
        where: {
          project_id: Number(project_id),
          project_role_data: {
            role_name: role_name,
          },
          is_delete: false,
        },
        select: {
          user_data: {
            select: {
              user_id: true,
              first_name: true,
              last_name: true,
              email_id: true,
              user_profiles: {
                select: {
                  profile_image_url: true,
                },
              },
            },
          },
          project_role_data: {
            select: { role_name: true },
          },
        },
        orderBy: [{ updated_date: 'desc' }],
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation getByProjectIdAndRoleType dao',
      error
    );
    throw error;
  }
};

const getByUserIdAndProjectRoleId = async (
  user_id: number,
  project_role_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectMemberAssociation =
      await transaction.project_member_association.findMany({
        where: {
          user_id: Number(user_id),
          project_role_id: Number(project_role_id),
          is_delete: false,
        },
        include: {
          project_data: true,
          user_data: {
            select: {
              first_name: true,
              last_name: true,
              email_id: true,
              user_profiles: {
                select: {
                  profile_image_url: true,
                },
              },
            },
          },
          project_role_data: { select: { role_name: true } },
        },
        orderBy: [{ updated_date: 'desc' }],
      });
    return projectMemberAssociation;
  } catch (error) {
    console.log(
      'Error occurred in projectMemberAssociation getByUserIdAndProjectRoleId dao',
      error
    );
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteProjectMemberAssociation,
  getByProjectIdAndUserId,
  getByProjectId,
  searchProjectMemberAssociation,
  getByProjectIdAndRoleType,
  getByUserIdAndProjectRoleId,
};
