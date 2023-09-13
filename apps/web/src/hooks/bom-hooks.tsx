import { useQuery, useMutation, useQueryClient } from 'react-query';
import BomService from '../service/bom-service';

const createBom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return BomService.createBomData(data);
    },
    {
      onSuccess: (response, _var) => {
        response;
        queryClient.invalidateQueries([
          'getOneSubcategoryID',
          _var[0].sub_category_id,
        ]);
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
