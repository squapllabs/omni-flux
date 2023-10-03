import prisma from '../utils/prisma';

const getById = async (bom_configuration_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const bomConfiguration = await transaction.bom_configuration.findFirst({
      where: {
        bom_configuration_id: Number(bom_configuration_id),
        is_delete: false,
      },
    });
    return bomConfiguration;
  } catch (error) {
    console.log('Error occurred in bom configuration dao getById', error);
    throw error;
  }
};

const updateBudget = async (
  budget: number,
  bom_configuration_id: number,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const currentDate = new Date();
    const bomConfiguration = await transaction.bom_configuration.update({
      where: {
        bom_configuration_id: bom_configuration_id,
      },
      data: {
        budget: budget,
        updated_date: currentDate,
        updated_by,
      },
    });
    return bomConfiguration;
  } catch (error) {
    console.log('Error occurred in bom configuration dao updateBudget', error);
    throw error;
  }
};

const add = async (
  bom_name: string,
  bom_description: string,
  bom_type_id: number,
  project_id: number,
  budget: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bomConfiguration = await transaction.bom_configuration.create({
      data: {
        bom_name,
        bom_description,
        budget,
        bom_type_id,
        project_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return bomConfiguration;
  } catch (error) {
    console.log('Error occurred in bomConfiguration add', error);
    throw error;
  }
};

const edit = async (
  bom_configuration_id: number,
  bom_name: string,
  bom_description: string,
  bom_type_id: number,
  project_id: number,
  budget: number,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bomConfiguration = await transaction.bom_configuration.update({
      where: {
        bom_configuration_id: bom_configuration_id,
      },
      data: {
        bom_name,
        bom_description,
        bom_type_id,
        project_id,
        budget,
        updated_by,
        updated_date: currentDate,
      },
    });
    return bomConfiguration;
  } catch (error) {
    console.log('Error occurred in bomConfiguration edit', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bomConfiguration = await transaction.bom_configuration.findMany({
      where: {
        is_delete: false,
      },
      include: {
        bom_type_data: {
          select: {
            master_data_name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return bomConfiguration;
  } catch (error) {
    console.log('Error occurred in bomConfiguration getAll dao', error);
    throw error;
  }
};

const deleteBomConfiguration = async (
  bom_configuration_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bomConfiguration = await transaction.bom_configuration.update({
      where: {
        bom_configuration_id: Number(bom_configuration_id),
      },
      data: {
        is_delete: true,
      },
    });
    return bomConfiguration;
  } catch (error) {
    console.log(
      'Error occurred in bomconfiguration deleteBomConfiguration dao',
      error
    );
    throw error;
  }
};

const getByBomConfigurationId = async (
  bom_configuration_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bomConfiguration = await transaction.bom_configuration.findFirst({
      where: {
        bom_configuration_id: Number(bom_configuration_id),
        is_delete: false,
      },
      include: {
        bom_type_data: {
          select: {
            master_data_name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
    });
    return bomConfiguration;
  } catch (error) {
    console.log(
      'Error occurred in bomConfiguration getByBomConfigurationId dao',
      error
    );
    throw error;
  }
};

const searchBomConfiguration = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterBomConfiguration;
    const bomConfiguration = await transaction.bom_configuration.findMany({
      where: filter,
      include: {
        bom_type_data: {
          select: {
            master_data_name: true,
          },
        },
        project_data: {
          select: { project_name: true },
        },
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const bomConfigurationCount = await transaction.bom_configuration.count({
      where: filter,
    });
    const bomConfigurationData = {
      count: bomConfigurationCount,
      data: bomConfiguration,
    };
    return bomConfigurationData;
  } catch (error) {
    console.log(
      'Error occurred in bomConfiguration dao : searchbomConfiguration',
      error
    );
    throw error;
  }
};

export default {
  getById,
  updateBudget,
  add,
  edit,
  getAll,
  deleteBomConfiguration,
  getByBomConfigurationId,
  searchBomConfiguration,
};
