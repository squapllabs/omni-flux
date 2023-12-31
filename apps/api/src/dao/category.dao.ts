import { createCategoryBody } from '../interfaces/category.Interface';
import prisma from '../utils/prisma';

const add = async (
  name: string,
  project_id: number,
  actual_budget: number,
  estimated_budget: number,
  created_by: bigint,
  description: string,
  start_date: Date,
  end_date: Date,
  bom_configuration_id: number,
  progress_status: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const category = await transaction.category.create({
      data: {
        name,
        project_id,
        actual_budget,
        estimated_budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
        description,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        bom_configuration_id,
        progress_status,
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
  actual_budget: number,
  estimated_budget: number,
  updated_by: bigint,
  category_id: number,
  description: string,
  start_date: Date,
  end_date: Date,
  bom_configuration_id: number,
  progress_status: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.update({
      where: {
        category_id: category_id,
      },
      data: {
        name,
        project_id,
        actual_budget,
        estimated_budget,
        updated_by,
        updated_date: currentDate,
        description,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        bom_configuration_id,
        progress_status,
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
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
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
      orderBy: [{ created_date: 'asc' }],
      include: {
        project: true,
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
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
      orderBy: [{ created_date: 'asc' }],
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
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
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

const getByProjectId = async (
  project_id: number,
  bom_configuration_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.findMany({
      where: {
        project_id: Number(project_id),
        bom_configuration_id: Number(bom_configuration_id),
        is_delete: false,
      },
      orderBy: [{ created_date: 'asc' }],
      include: {
        project: true,
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
      },
    });
    return category;
  } catch (error) {
    console.log('Error occurred in category getByProjectId dao', error);
    throw error;
  }
};

const updateBudget = async (
  budget: number,
  category_id: number,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const currentDate = new Date();
    const subCategoryDetails = await transaction.category.update({
      where: {
        category_id: category_id,
      },
      data: {
        actual_budget: budget,
        updated_date: currentDate,
        updated_by,
      },
    });
    return subCategoryDetails;
  } catch (error) {
    console.log('Error occurred in category dao updateBudget', error);
    throw error;
  }
};

const getCountByProjectIdAndBomConfigId = async (
  project_id: number,
  bom_configuration_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const category = await transaction.category.count({
      where: {
        project_id: Number(project_id),
        bom_configuration_id: Number(bom_configuration_id),
        is_delete: false,
      },
    });

    const subCategory = await transaction.sub_category.count({
      where: {
        project_id: Number(project_id),
        bom_configuration_id: Number(bom_configuration_id),
        is_delete: false,
      },
    });

    const bomConfiguration = await transaction.bom_configuration.findFirst({
      where: {
        project_id: Number(project_id),
        bom_configuration_id: Number(bom_configuration_id),
        is_delete: false,
      },
      include: {
        project_data: { select: { project_name: true } },
        bom_type_data: {
          select: {
            master_data_name: true,
          },
        },
      },
    });
    const result = {
      abstract_count: category,
      tasks_count: subCategory,
      bom_configuration_data: bomConfiguration,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in category getCountByProjectIdAndBomConfigId dao',
      error
    );
    throw error;
  }
};

const addBulk = async (
  categories: createCategoryBody[],
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const currentDate = new Date();
    const categoryData = categories.map((categoryObj) => ({
      name: categoryObj.name,
      project_id: categoryObj.project_id,
      estimated_budget: categoryObj.estimated_budget,
      created_by: categoryObj.created_by,
      description: categoryObj.description,
      start_date: categoryObj.start_date
        ? new Date(categoryObj.start_date)
        : null,
      end_date: categoryObj.end_date ? new Date(categoryObj.end_date) : null,
      bom_configuration_id: categoryObj.bom_configuration_id,
      progress_status: categoryObj.progress_status,
      created_date: currentDate,
      updated_date: currentDate,
      is_delete: false,
    }));

    const createdCategories = await transaction.category.createMany({
      data: categoryData,
    });
    return createdCategories;
  } catch (error) {
    console.log('Error occurred in categoryDao addBulk ', error);
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
  getByProjectId,
  updateBudget,
  getCountByProjectIdAndBomConfigId,
  addBulk,
};
