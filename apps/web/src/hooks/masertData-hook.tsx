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

const getBymasertDataID = (id: number) => {
  return useQuery(
    ['getOnemasertDataID', id],
    () => masertDataService.getOnemasertDataByID(id),
    {
      select: (data) => data.data,
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
};
