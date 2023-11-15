import { useQuery, useMutation, useQueryClient } from 'react-query';
import masertDataService from '../service/masterData-service';

const useGetAllmasertData = () => {
  return useQuery(
    ['useGetAllmasertData'],
    () => masertDataService.getAllmasertData(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetAllmasertDataDrop = () => {
  return useQuery(
    ['useGetAllmasertData'],
    () => masertDataService.getAllmasertData(),
    {
      select: (data) =>
        data?.data?.map((category: any) => ({
          value: category.sub_category_id,
          label: category.name,
        })),
    }
  );
};
const useGetAllParentmasertDataDrop = () => {
  return useQuery(
    ['useGetAllParentMasterData'],
    () => masertDataService.getAllParentMasterData(),
    {
      select: (data) =>
        data?.data?.map((category: any) => ({
          label: category.master_data_name.toUpperCase(),
          value: category.master_data_id,
        })),
    }
  );
};

const getBymasertDataID = (id: number) => {
  return useQuery(
    ['getOnemasertDataID', id],
    () => masertDataService.getOnemasertDataByID(id),
    {
      select: (data) => data.data,
    }
  );
};
const getBymasertDataType = (value: String) => {
  return useQuery(
    ['getOnemasertDataType', value],
    () => masertDataService.getOnemasertDataByType(value),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};
const getBymasertDataTypeDrop = (value: String) => {
  return useQuery(
    ['getBymasertDataTypeDrop', value],
    () => masertDataService.getOnemasertDataByType(value),
    {
      select: (data) =>
        data?.data?.map((option: any) => ({
          value: option.master_data_id,
          label: option.master_data_name,
        })),
      staleTime: Infinity,
    }
  );
};

const getByMasterDataProjectIdDrop = (value: String) => {
  return useQuery(
    ['getByMasterDataProjectIdDrop', value],
    () => masertDataService.getOneMasterDataByProjectId(value),
    {
      select: (data) =>
        data?.data?.map((option: any) => ({
          value: option.master_data_id,
          label: option.master_data_name,
        })),
      staleTime: Infinity,
    }
  );
};

const createmasertData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return masertDataService.createmasertData(data);
    },
    {
      onSuccess: (data) => {
        queryClient.invalidateQueries(['useGetAllMasterPaginatedData']);
        queryClient.invalidateQueries([
          'getByMasterDataProjectIdDrop',
          data?.data?.project_id,
        ]);
      },
    }
  );
};

const updatemasertData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return masertDataService.updatemasertData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllMasterPaginatedData']);
      },
    }
  );
};

const useDeletemasertData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return masertDataService.deletemasertData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllMasterPaginatedData']);
      },
    }
  );
};

const useGetMasterCurency = () => {
  return useQuery(
    ['useGetAllmasertData'],
    () => masertDataService.getAllCurrencyData(),
    {
      select: (data) =>
        data?.data?.map((currency: any) => ({
          value: currency.master_data_name,
          label: currency.master_data_name,
        })),
    }
  );
};

const getBySearchmasterData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return masertDataService.filtermasertData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};
//
const useGetAllPaginatedMasterData = (data: any) => {
  return useQuery(
    ['useGetAllMasterPaginatedData'],
    () => masertDataService.filtermasertData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};
export {
  useGetAllmasertData,
  getBymasertDataID,
  createmasertData,
  updatemasertData,
  useDeletemasertData,
  getBySearchmasterData,
  useGetAllmasertDataDrop,
  useGetAllParentmasertDataDrop,
  useGetMasterCurency,
  getBymasertDataType,
  useGetAllPaginatedMasterData,
  getBymasertDataTypeDrop,
  getByMasterDataProjectIdDrop,
};
