import { useQuery, useMutation, useQueryClient } from 'react-query';
import MachineryService from '../service/machinery-service';

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

export { useGetAllMachineryForDrop };
