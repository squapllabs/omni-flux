import addProduct from '../service/add-product';
import {useQuery, useMutation,useQueryClient } from 'react-query';

const createItem = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return addProduct.addProduct(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllItem']);
        },
      }
    );
  };

  const useGetAllItem = () => {
    return useQuery(['useGetAllItem'], () => addProduct.getAllItems(), {
      select: (data) => data.data,
      staleTime: Infinity,
    });
  };

  const updateItem = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data:any) => {
        return addProduct.updateItem(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllItem']);
        },
      }
    );
  };

  const useGetAllPaginatedItemData = (data: any) => {
    return useQuery(
      ['useGetAllItem'],
      () => addProduct.filterItem(data),
      {
        select: (data) => data,
        staleTime: Infinity,
      }
    );
  };

  const getByItem = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return addProduct.filterItem(data);
      },
      {
        onSuccess: (response) => {
          response;
        },
      }
    );
  };

  

  export {createItem, updateItem,getByItem,useGetAllItem,useGetAllPaginatedItemData};