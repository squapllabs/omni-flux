import { useQuery, useMutation, useQueryClient } from 'react-query';
import MachineryService from '../service/machinery-service';

const useGetAllMachinery = () => {
    return useQuery(
      ['useGetAllMachinery'],
      () => MachineryService.getAllMachinery(),
      {
        select: (data) => data.data,
        staleTime: Infinity,
      }
    );
  };

  const createMachinery = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return MachineryService.createMachinery(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllMachinery']);
        },
      }
    );
  };

  const createInstantMachinery = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return MachineryService.createMachinery(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllMAchineryDrop']);
        },
      }
    );
  };

  const useGetAllPaginatedMachinery = (data: any) => {
    return useQuery(
      ['useGetAllMachinery'],
      () => MachineryService.filterMachinery(data),
      {
        select: (data) => data,
        staleTime: Infinity,
      }
    );
  };

  const getByMachinery = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data:any) => {
        return MachineryService.filterMachinery(data);
      },
      {
        onSuccess: (response) => {
          response;
        },
      }
    )
  };

  const getByMachineryID = (id: number) => {
    return useQuery(
      ['getByMachineryID', id],
      () => MachineryService.getOneMachineryByID(id),
      {
        select: (data) => data.data,
      }
    );
  };

  const updateMachinery = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return MachineryService.updateMachinery(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllMachinery']);
        },
      }
    );
  };

  const useDeleteMachinery = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return MachineryService.deleteMachinery(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllMachinery']);
        },
      }
    );
  };
  

  const useGetAllMachineryForDrop = () => {
    return useQuery(
      ['useGetAllMAchineryDrop'],
      () => MachineryService.getAllMachinery(),
      {
        select: (data) =>
          data?.data?.map((option: any) => ({
            value: option.machinery_id,
            label: option.machinery_name,
            data: option,
          })),
      }
    );
  };

  export {useGetAllMachinery,createMachinery,createInstantMachinery,useGetAllPaginatedMachinery,getByMachinery,getByMachineryID,updateMachinery,useDeleteMachinery,useGetAllMachineryForDrop};