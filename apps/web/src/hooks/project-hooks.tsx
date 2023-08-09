import { useQuery, useMutation, useQueryClient } from 'react-query';
import ProjectService from '../service/project-service';

const useGetAllProject = () => {
  return useQuery(['useGetAllProject'], () => ProjectService.getAllProject(), {
    select: (data) =>
      data?.data?.map((project: any) => ({
        value: project.project_id,
        label: project.project_name,
      })),
  });
};

const createProject = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.createProjectData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries([]);
      },
    }
  );
};

const getByProject = () => {
  const queryClient = useQueryClient();
  return useMutation (
    (data:any) => {
      return ProjectService.filterProject(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  )
}

export { useGetAllProject,createProject,getByProject };
