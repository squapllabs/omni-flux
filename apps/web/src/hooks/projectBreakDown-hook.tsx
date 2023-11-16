import { useQuery, useMutation, useQueryClient } from 'react-query';
import projectBreakDownService from '../service/projectBreakdown-service';

const useGetAllParentProjectBreakDownDrop = () => {
  return useQuery(
    ['useGetAllParentProjectBreakDown'],
    () => projectBreakDownService.getAllParentProjectBreakDown(),
    {
      select: (data) =>
        data?.data?.map((parent: any) => ({
          label: parent.project_workbreak_down_name,
          value: parent.project_workbreak_down_id,
        })),
    }
  );
};

const useCreateProjectBreakDownData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectBreakDownService.createProjectBreakDownData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};

const useGetBySearchProjectWorkBreakDownData = () => {
  return useMutation((data: any) => {
    return projectBreakDownService.filterProjectWorkBreakDownData(data);
  });
};

const useGetByProjectWorkBreakDownId = (id: number) => {
  return useQuery(
    ['getByuserID', id],
    () => projectBreakDownService.getOneProjectWorkBreakDownById(id),
    {
      select: (data) => data.data,
    }
  );
};

const useUpdateProjectBreakDown = () => {
  return useMutation((data: any) => {
    return projectBreakDownService.updateProjectBreakDownData(data);
  });
};
export {
  useGetAllParentProjectBreakDownDrop,
  useCreateProjectBreakDownData,
  useGetBySearchProjectWorkBreakDownData,
  useGetByProjectWorkBreakDownId,
  useUpdateProjectBreakDown,
};
