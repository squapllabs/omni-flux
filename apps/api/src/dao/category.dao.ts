import prisma from '../utils/prisma';

const add = async (
  name: string,
  project_id: number,
  budget: number,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.create({
      data: {
        name,
        project_id,
        budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return category;
  } catch (error) {
    console.log('Error occurred in categoryDao add', error);
    throw error;
  }
};

const edit = async (
  name: string,
  project_id: number,
  budget: number,
  updated_by: bigint,
  category_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.update({
      where: {
        category_id: category_id,
      },
      data: {
        name,
        project_id,
        budget,
        updated_by,
        updated_date: currentDate,
      },
    });
    return category;
  } catch (error) {
    console.log('Error occurred in categoryDao edit', error);
    throw error;
  }
};

const getById = async (categoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.findUnique({
      where: {
        category_id: Number(categoryId),
      },
    });
    return category;
  } catch (error) {
    console.log('Error occurred in category getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
      include: {
        project: true,
      },
    });
    return category;
  } catch (error) {
    console.log('Error occurred in category getAll dao', error);
    throw error;
  }
};

const deleteCategory = async (categoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.delete({
      where: {
        category_id: Number(categoryId),
      },
    });
    return category;
  } catch (error) {
    console.log('Error occurred in category deleteCategory dao', error);
    throw error;
  }
};

const getByCategoryNameAndProjectId = async (
  categoryName: string,
  projectId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category =
      await transaction.$queryRaw`select * from category c where lower(c."name") = lower(${categoryName}) and c.project_id =${projectId}`;
    return category[0];
  } catch (error) {
    console.log(
      'Error occurred in category getByCategoryNameAndProjectId dao',
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
  deleteCategory,
  getByCategoryNameAndProjectId,
};
