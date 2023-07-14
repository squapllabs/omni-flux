import { useQuery, useMutation, useQueryClient } from 'react-query';
import gstService from '../service/gst-service';

const useGetAllGst = () => {
  return useQuery(['useGetAllGst'], () => gstService.getAllGst(), {
    select: (data) => data.data,
  });
};

const useGetOneGst = (id : number) => {
  return useQuery(['useGetOneGst'], () => gstService.getOneGst(id), {
    select: (data) => data.data,
  });
};

const useDeleteGst = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return gstService.deleteGst(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllGst']);
        },
      }
    );
  };

  const createGst = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return gstService.createGst(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllGst']);
        },
      }
    );
  };

  const updateGst = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return gstService.updateGst(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllGst']);
        },
      }
    );
  };
export {
    useGetAllGst,
    useDeleteGst,
    createGst,
    updateGst,
    useGetOneGst
  };
  