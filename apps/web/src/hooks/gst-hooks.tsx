import { useQuery, useMutation, useQueryClient } from 'react-query';
import gstService from '../service/gst-service';

const useGetAllGst = () => {
  return useQuery(['useGetAllGst'], () => gstService.getAllGst(), {
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
    return useMutation({
      mutationFn: gstService.createGst,
    });
  };

  const updateGst = () => {
    return useMutation({
      mutationFn: gstService.updateGst,
    });
  };

export {
    useGetAllGst,
    useDeleteGst,
    createGst,
    updateGst
  };
  