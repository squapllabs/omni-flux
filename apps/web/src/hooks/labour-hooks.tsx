import { useQuery, useMutation, useQueryClient } from 'react-query';
import LabourService from '../service/labour-service';

const useGetAllLabourForDrop = () => {
  return useQuery(
    ['useGetAllLabourDrop'],
    () => LabourService.getAllLabours(),
    {
      select: (data) =>
        data?.data?.map((option: any) => ({
          value: option.labour_id,
          label: option.labour_type,
          data: option,
        })),
    }
  );
};

export { useGetAllLabourForDrop };
