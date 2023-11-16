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

const useGetBymasertDataID = (id: number) => {
  return useQuery(
    ['getOnemasertDataID', id],
    () => masertDataService.getOnemasertDataByID(id),
    {
      select: (data) => data.data,
    }
  );
};
const useGetBymasertDataType = (value: any) => {
  return useQuery(
    ['getOnemasertDataType', value],
    () => masertDataService.getOnemasertDataByType(value),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};
const useGetBymasertDataTypeDrop = (value: any) => {
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

const useGetByMasterDataProjectIdDrop = (value: any) => {
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

const useCreatemasterData = () => {
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

const useUpdatemasterData = () => {
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

const useGetBySearchmasterData = () => {
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
  useGetBymasertDataID,
  useCreatemasterData,
  useUpdatemasterData,
  useDeletemasertData,
  useGetBySearchmasterData,
  useGetAllmasertDataDrop,
  useGetAllParentmasertDataDrop,
  useGetMasterCurency,
  useGetBymasertDataType,
  useGetAllPaginatedMasterData,
  useGetBymasertDataTypeDrop,
  useGetByMasterDataProjectIdDrop,
};
