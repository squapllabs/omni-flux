import { useQuery, useMutation, useQueryClient } from 'react-query';
import BomService from '../service/bom-service';

const createBom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return BomService.createBomData(data);
    },
    {
      onSuccess: (response) => {
        response;
        //   queryClient.invalidateQueries(['useGetAllClientData']);
      },
    }
  );
};
const createBulkBom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return BomService.createBulkBom(data);
    },
    {
      onSuccess: (response) => {
        response;
        //   queryClient.invalidateQueries(['useGetAllClientData']);
      },
    }
  );
};

export { createBom, createBulkBom };
