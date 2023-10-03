import bomConfigurationDao from '../dao/bomConfiguration.dao';
import { bomConfigurationBody } from '../interfaces/bomConfiguration.interface';

const createBomConfiguration = async (body: bomConfigurationBody) => {
  try {
    const {
      bom_name,
      bom_description,
      budget,
      bom_type_id,
      project_id,
      created_by,
    } = body;
    const bomConfigurationDetails = await bomConfigurationDao.add(
      bom_name,
      bom_description,
      budget,
      bom_type_id,
      project_id,
      created_by
    );
    const result = {
      message: 'success',
      status: true,
      data: bomConfigurationDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in bomConfiguration service Add: ', error);
    throw error;
  }
};

const updateBomConfiguration = async (body: bomConfigurationBody) => {
  try {
    const {
      bom_configuration_id,
      bom_name,
      bom_description,
      budget,
      bom_type_id,
      project_id,
      updated_by,
    } = body;
    let result = null;
    const bomConfigurationExists = await bomConfigurationDao.getById(
      bom_configuration_id
    );
    if (!bomConfigurationExists) {
      result = {
        message: 'bom_configuration_id does not exist',
        status: false,
        data: null,
      };
      return result;
    } else {
      const bomConfigurationDetails = await bomConfigurationDao.edit(
        bom_configuration_id,
        bom_name,
        bom_description,
        budget,
        bom_type_id,
        project_id,
        updated_by
      );
      result = {
        message: 'success',
        status: true,
        data: bomConfigurationDetails,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in bomConfiguration service Edit: ', error);
    throw error;
  }
};

const getAllBomConfiguration = async () => {
  try {
    const result = await bomConfigurationDao.getAll();
    const bomConfigurationDetails = {
      message: 'success',
      status: true,
      data: result,
    };
    return bomConfigurationDetails;
  } catch (error) {
    console.log('Error occurred in getAll bomConfiguration service : ', error);
    throw error;
  }
};

const deleteBomConfiguration = async (bom_configuration_id: number) => {
  try {
    const bomConfigurationExists = await bomConfigurationDao.getById(
      bom_configuration_id
    );

    if (!bomConfigurationExists) {
      const result = {
        message: 'bom_configuration_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await bomConfigurationDao.deleteBomConfiguration(
      bom_configuration_id
    );
    if (data) {
      const result = {
        message: 'BomConfiguration Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this bomConfiguration detail',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in bomConfiguration delete service : ', error);
    throw error;
  }
};

const getByBomConfigurationId = async (bom_configuration_id: number) => {
  try {
    let result = null;
    const bomConfigurationDetails =
      await bomConfigurationDao.getByBomConfigurationId(bom_configuration_id);
    if (bomConfigurationDetails) {
      result = {
        message: 'success',
        status: true,
        data: bomConfigurationDetails,
      };
      return result;
    } else {
      result = {
        message: 'bom_configuration_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByBomConfigurationId bomConfigurationDetails service : ',
      error
    );
    throw error;
  }
};

export = {
  createBomConfiguration,
  updateBomConfiguration,
  getAllBomConfiguration,
  deleteBomConfiguration,
  getByBomConfigurationId,
};
