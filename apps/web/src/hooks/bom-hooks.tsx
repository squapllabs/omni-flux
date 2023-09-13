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

const getBOMbyProjectandType = (value: any) => {
  return useQuery(
    ['getBOMbyProjectandType', value],
    () => BomService.getBOMbyProjectandType(value),
    {
      select: (data) =>
        data?.data?.map((option: any) => ({
          value: option.bom_detail_id,
          label: option.item_data?.item_name,
          item_rate: option.item_data?.rate,
          bom_rate: option.rate,
          bom_quantity: option.quantity,
          temp: option,
        })),
    }
  );
};

export { createBom, createBulkBom, getBOMbyProjectandType };
