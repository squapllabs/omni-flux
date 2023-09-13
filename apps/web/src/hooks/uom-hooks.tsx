import { useQuery, useMutation, useQueryClient } from 'react-query';
import uomService from '../service/uom-service';

const useGetAlluom = () => {
  return useQuery(['useGetAlluom'], () => uomService.getAlluom(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const getByuserID = (id: number) => {
  return useQuery(['getOneUomByID', id], () => uomService.getOneUomByID(id), {
    select: (data) => data.data,
  });
};

const createuom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return uomService.createUom(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAlluom']);
      },
    }
  );
};

const updateUom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return uomService.updateUom(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAlluom']);
      },
    }
  );
};

const useDeleteUom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return uomService.deleteUom(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAlluom']);
      },
    }
  );
};

export { useGetAlluom, getByuserID, createuom, updateUom, useDeleteUom };
