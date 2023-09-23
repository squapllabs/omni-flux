import { useQuery, useMutation, useQueryClient } from 'react-query';
import stockOutwardService from '../service/stock-outward-service';


const getAllStockOutwardData = () => {
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

const createStockOutWard = () => {
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

const getByStockOutWardId = (id: number) => {
  console.log("sad",id);
  
  return useQuery(
    ['getOneStockOutWardId', id],
    () => stockOutwardService.getOneStockOutWardId(id),
    {
      select: (data) => data.data,
    }
  );
};

const updateStockOutWard = () => {
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

export { getAllStockOutwardData, createStockOutWard, getByStockOutWardId,updateStockOutWard }