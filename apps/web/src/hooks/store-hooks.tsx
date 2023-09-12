import { useQuery, useMutation, useQueryClient } from 'react-query';
import StoreService from '../service/store-service';

const useGetAllStore = () => {
    return useQuery(
      ['useGetAllStore'],
      () => StoreService.getAllStore(),
      {
        select: (data) => data.data,
        staleTime: Infinity,
      }
    );
  };

const createStore = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return StoreService.createStore(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllCategory']);
        },
      }
    );
  };

  const updateStore = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return StoreService.updateStore(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllCategory']);
        },
      }
    );
  };

  const getBySearchStore = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return StoreService.filterStore(data);
      },
      {
        onSuccess: (response) => {
          response;
        },
      }
    );
  };

  const useGetAllPaginatedStoreData = (data: any) => {
    return useQuery(
      ['useGetAllStoreData'],
      () => StoreService.filterStore(data),
      {
        select: (data) => data,
        staleTime: Infinity,
      }
    );
  };

  const useDeleteStore = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return StoreService.deleteStore(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllStoreData']);
        },
      }
    );
  };

  const getByStoreID = (id: number) => {
    return useQuery(
      ['getOnecategoryID', id],
      () => StoreService.getOneStoreByID(id),
      {
        select: (data) => data.data,
      }
    );
  };

  export {createStore,useGetAllStore,updateStore,getBySearchStore,useDeleteStore,getByStoreID,useGetAllPaginatedStoreData};