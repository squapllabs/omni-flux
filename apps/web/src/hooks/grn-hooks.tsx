import { useQuery, useMutation, useQueryClient } from 'react-query';
import GrnService from '../service/grn-service';

const createGrn = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return GrnService.createGrnData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries([]);
      },
    }
  );
};

export { createGrn };
