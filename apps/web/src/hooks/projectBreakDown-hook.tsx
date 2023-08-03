import { useQuery, useMutation, useQueryClient } from 'react-query';
import projectBreakDownService from '../service/projectBreakdown-service';

const useGetAllParentProjectBreakDownDrop = () => {
  return useQuery(
    ['useGetAllParentProjectBreakDown'],
    () => projectBreakDownService.getAllParentProjectBreakDown(),
    {
      select: (data) =>
        data?.data?.map((category: any) => ({
          label: category.master_data_name.toUpperCase(),
          value: category.master_data_id,
        })),
    }
  );
};

const createProjectBreakDownData = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return projectBreakDownService.createProjectBreakDownData(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetAllParentProjectBreakDown']);
        },
      }
    );
  };

export {
    useGetAllParentProjectBreakDownDrop,
    createProjectBreakDownData
};
