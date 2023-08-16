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

const createmasertData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return masertDataService.createmasertData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllmasertData']);
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
        queryClient.invalidateQueries(['useGetAllmasertData']);
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
        queryClient.invalidateQueries(['useGetAllmasertData']);
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
};
