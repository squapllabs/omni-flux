import prisma from '../utils/prisma';

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

export default { updateBudget };
