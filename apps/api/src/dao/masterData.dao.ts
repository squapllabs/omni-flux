import prisma from '../utils/prisma';

const add = async (
  master_data_name: string,
  master_data_description: string,
  master_data_type: string,
  parent_master_data_id: number,
  project_id: number,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.create({
      data: {
        master_data_name,
        master_data_description,
        master_data_type,
        parent_master_data_id,
        project_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return masterData;
  } catch (error) {
    console.log('Error occurred in masterDataDao add', error);
    throw error;
  }
};

const edit = async (
  master_data_name: string,
  master_data_description: string,
  master_data_type: string,
  project_id: number,
  updated_by: bigint,
  master_data_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.update({
      where: {
        master_data_id: master_data_id,
      },
      data: {
        master_data_name,
        master_data_description,
        master_data_type,
        project_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return masterData;
  } catch (error) {
    console.log('Error occurred in masterDataDao edit', error);
    throw error;
  }
};

const getById = async (masterDataId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findFirst({
      where: {
        master_data_id: Number(masterDataId),
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
    });
    return masterData;
  } catch (error) {
    console.log('Error occurred in masterData getById dao', error);
    throw error;
  }
};

const getByParentMasterDataType = async (
  masterDataType: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findFirst({
      where: {
        master_data_type: masterDataType,
        parent_master_data_id: null,
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
    });
    return masterData;
  } catch (error) {
    console.log(
      'Error occurred in masterData getByParentMasterDataType dao',
      error
    );
    throw error;
  }
};

const getByParentMasterDataId = async (
  parentMasterDataId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findFirst({
      where: {
        parent_master_data_id: Number(parentMasterDataId),
      },
      include: {
        parent: true,
        children: true,
      },
    });
    return masterData;
  } catch (error) {
    console.log(
      'Error occurred in masterData getByParentMasterDataId dao',
      error
    );
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findMany({
      where: {
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return masterData;
  } catch (error) {
    console.log('Error occurred in masterData getAll dao', error);
    throw error;
  }
};

const getAllParentMasterData = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findMany({
      where: {
        parent_master_data_id: null,
        project_id: null,
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return masterData;
  } catch (error) {
    console.log(
      'Error occurred in masterData getAllParentMasterData dao',
      error
    );
    throw error;
  }
};

const deleteMasterData = async (masterDataId: number, connectionObj = null) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.update({
      where: {
        master_data_id: Number(masterDataId),
      },
      data: {
        is_delete: true,
        updated_date: currentDate,
      },
    });
    return masterData;
  } catch (error) {
    console.log('Error occurred in masterData deleteMasterData dao', error);
    throw error;
  }
};

const getByParentMasterDataIdAndType = async (
  parentMasterDataId: number,
  masterDataType: string,
  project_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const parent_master_data_id = parentMasterDataId
      ? Number(parentMasterDataId)
      : null;
    const masterData = await transaction.master_data.findFirst({
      where: {
        parent_master_data_id: parent_master_data_id,
        master_data_type: masterDataType,
        project_id: project_id,
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
    });
    return masterData;
  } catch (error) {
    console.log(
      'Error occurred in masterData getByParentMasterDataIdAndType dao',
      error
    );
    throw error;
  }
};

const searchMasterData = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterMasterData;
    if (filter.AND[0].project_id.equals === null) {
      const getData = await transaction.master_data.findMany({
        where: {
          is_delete: filter.is_delete,
        },
      });
      if (getData.length > 0) {
        const masterData = await transaction.master_data.findMany({
          where: filter,
          include: {
            parent: true,
            children: true,
          },
          orderBy: [
            {
              [orderByColumn]: orderByDirection,
            },
          ],
          skip: offset,
          take: limit,
        });
        const masterDataCount = await transaction.master_data.count({
          where: filter,
        });
        const masterDataData = {
          count: masterDataCount,
          data: masterData,
        };
        return masterDataData;
      } else {
        return getData;
      }
    } else {
      const getData = await transaction.master_data.findMany({
        where: {
          is_delete: filter.is_delete,
          project_id: filter.AND[0].project_id,
        },
      });
      if (getData.length > 0) {
        const masterData = await transaction.master_data.findMany({
          where: filter,
          include: {
            parent: true,
            children: true,
          },
          orderBy: [
            {
              [orderByColumn]: orderByDirection,
            },
          ],
          skip: offset,
          take: limit,
        });
        const masterDataCount = await transaction.master_data.count({
          where: filter,
        });
        const masterDataData = {
          count: masterDataCount,
          data: masterData,
        };
        return masterDataData;
      } else {
        return getData;
      }
    }
  } catch (error) {
    console.log('Error occurred in masterData dao : searchMasterData ', error);
    throw error;
  }
};

const getByParentType = async (
  masterDataType: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterDataId = await transaction.master_data.findFirst({
      where: {
        master_data_type: masterDataType,
      },
      select: {
        master_data_id: true,
      },
    });

    const masterDataIdOfType = masterDataId?.master_data_id;

    if (masterDataIdOfType) {
      const result = await transaction.master_data.findMany({
        where: {
          parent_master_data_id: {
            in: masterDataIdOfType,
          },
        },
      });
      return result;
    } else {
      return null;
    }
  } catch (error) {
    console.log('Error occurred in masterData getByParentType dao', error);
    throw error;
  }
};

const getByProjectId = async (project_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findMany({
      where: {
        project_id: Number(project_id),
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return masterData;
  } catch (error) {
    console.log('Error occurred in masterData getByProjectId dao', error);
    throw error;
  }
};

const getAllProjectMasterData = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findMany({
      where: {
        project_id: {
          not: {
            equals: null,
          },
        },
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return masterData;
  } catch (error) {
    console.log(
      'Error occurred in masterData getAllProjectMasterData dao',
      error
    );
    throw error;
  }
};

const getByProjectIdAndType = async (
  project_id: number,
  masterDataType: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findFirst({
      where: {
        project_id: Number(project_id),
        master_data_type: masterDataType,
        is_delete: false,
      },
      include: {
        parent: true,
        children: true,
      },
    });
    return masterData;
  } catch (error) {
    console.log(
      'Error occurred in masterData getByProjectIdAndType dao',
      error
    );
    throw error;
  }
};

const getByMasterDataName = async (
  master_data_name: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const masterData = await transaction.master_data.findFirst({
      where: {
        project_id: null,
        is_delete: false,
        master_data_name: master_data_name,
        parent_master_data_id: {
          not: { equals: null },
        },
      },
      include: {
        parent: true,
        children: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return masterData;
  } catch (error) {
    console.log('Error occurred in masterData getByMasterDataName dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getByParentMasterDataType,
  getAll,
  deleteMasterData,
  getAllParentMasterData,
  getByParentMasterDataId,
  getByParentMasterDataIdAndType,
  searchMasterData,
  getByParentType,
  getByProjectId,
  getAllProjectMasterData,
  getByProjectIdAndType,
  getByMasterDataName,
};
