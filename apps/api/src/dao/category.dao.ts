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
    const is_delete = false;
    const category = await transaction.category.create({
      data: {
        name,
        project_id,
        budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
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
    const category = await transaction.category.findFirst({
      where: {
        category_id: Number(categoryId),
        is_delete: false,
      },
      include: {
        project: true,
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
      where: {
        is_delete: false,
      },
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
    const currentDate = new Date();
    const category = await transaction.category.update({
      where: {
        category_id: Number(categoryId),
      },
      data: {
        is_delete: true,
        updated_date: currentDate,
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
      await transaction.$queryRaw`select * from category c where lower(c."name") = lower(${categoryName}) and c.project_id =${projectId} and c.is_delete =false`;
    return category[0];
  } catch (error) {
    console.log(
      'Error occurred in category getByCategoryNameAndProjectId dao',
      error
    );
    throw error;
  }
};

const getAllInActiveCategories = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.findMany({
      where: {
        is_delete: true,
      },
    });
    return category;
  } catch (error) {
    console.log(
      'Error occurred in category getAllInActiveCategories dao',
      error
    );
    throw error;
  }
};

const searchCategory = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterCategory;
    const category = await transaction.category.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      include: {
        project: true,
      },
      skip: offset,
      take: limit,
    });
    const categoryCount = await transaction.category.count({
      where: filter,
    });
    const categoryData = {
      count: categoryCount,
      data: category,
    };
    return categoryData;
  } catch (error) {
    console.log('Error occurred in category dao : searchCategory ', error);
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
  getAllInActiveCategories,
  searchCategory,
};
