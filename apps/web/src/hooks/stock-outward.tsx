import { useQuery, useMutation, useQueryClient } from 'react-query';
import stockOutwardService from '../service/stock-outward-service';

const useGetAllStockOutwardData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return stockOutwardService.getStockOutWardData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useGetAllPaginatedStockOutwardData = (data: any) => {
  return useQuery(
    ['getAllPaginatedStockOutwardData'],
    () => stockOutwardService.getStockOutWardData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useCreateStockOutWard = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return stockOutwardService.addStockOutWard(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};

const useGetByStockOutWardId = (id: number) => {
  return useQuery(
    ['getOneStockOutWardId', id],
    () => stockOutwardService.getOneStockOutWardId(id),
    {
      select: (data) => data.data,
    }
  );
};

const useUpdateStockOutWard = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return stockOutwardService.updateStockOutWard(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};

export {
  useGetAllStockOutwardData,
  useCreateStockOutWard,
  useGetByStockOutWardId,
  useUpdateStockOutWard,
  useGetAllPaginatedStockOutwardData,
};
