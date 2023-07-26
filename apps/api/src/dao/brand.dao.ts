import prisma from '../utils/prisma';

const add = async (
  brand_id: number,
  brand_name: string,
  created_by: bigint,
  updated_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const brand = await transaction.brand.create({
      data: {
        brand_id,
        brand_name,
        created_by,
        updated_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return brand;
  } catch (error) {
    console.log('Error occurred in brandDao add dao', error);
    throw error;
  }
};
const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const totalCount = await transaction.item.count({
    });
    const brands = await transaction.brand.findMany({});
    return {brands, totalCount};
  } catch (error) {
    console.log('Error occurred in brand getAll dao', error);
    throw error;
  }
};
const getById = async (brand_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const brand = await transaction.brand.findUnique({
      where: {
        brand_id: Number(brand_id),
      },
    });
    return brand;
  } catch (error) {
    console.log('Error occurred in brand getById dao', error);
    throw error;
  }
};
const deleteBrand = async (brand_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const brand = await transaction.brand.delete({
      where: {
        brand_id: Number(brand_id),
      },
    });
    return brand;
  } catch (error) {
    console.log('Error occurred in brand delete dao', error);
    throw error;
  }
};
const edit = async (
  brand_id: number,
  brand_name: string,
  updated_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const brand = await transaction.brand.update({
      where: {
        brand_id: brand_id,
      },
      data: {
        brand_id,
        brand_name,
        updated_by,
        updated_date: currentDate,
      },
    });
    return brand;
  } catch (error) {
    console.log('Error occurred in brandDao edit', error);
    throw error;
  }
};

export default {
  add,
  getAll,
  getById,
  deleteBrand,
  edit,
};
