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

const createProjectBreakDownData = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return projectBreakDownService.createProjectBreakDownData(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries([]);
        },
      }
    );
  };

  const getBySearchProjectWorkBreakDownData = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return projectBreakDownService.filterProjectWorkBreakDownData(data);
      },
      {
        onSuccess: (response) => {
          response;
        },
      }
    );
  };

  const getByProjectWorkBreakDownId = (id: number) => {
    return useQuery(['getByuserID', id], () => projectBreakDownService.getOneProjectWorkBreakDownById(id), {
      select: (data) => data.data,
    });
  };
  
  const updateProjectBreakDown = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return projectBreakDownService.updateProjectBreakDownData(data);
      },
      {
        onSuccess: (response) => {
          response;
        },
      }
    );
  };
export {
    useGetAllParentProjectBreakDownDrop,
    createProjectBreakDownData,
    getBySearchProjectWorkBreakDownData,
    getByProjectWorkBreakDownId,
    updateProjectBreakDown
};
