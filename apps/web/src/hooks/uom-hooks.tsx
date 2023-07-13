import { useQuery, useMutation, useQueryClient } from 'react-query';
import uomService from '../service/uom-service';

const useGetAlluom = () => {
  return useQuery(['useGetAlluom'], () => uomService.getAlluom(), {
    select: (data) => data.data,
  });
};

const getByuserID = (id: number) => {
  return useQuery(['getOneUomByID', id], () => uomService.getOneUomByID(id), {
    select: (data) => data.data,
  });
};
const createuom = () => {
  return useMutation({
    mutationFn: uomService.createUom,
  });
};
const updateUom = () => {
  return useMutation({
    mutationFn: uomService.updateUom,
  });
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
