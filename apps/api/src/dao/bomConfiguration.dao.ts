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

export default { getById, updateBudget };
