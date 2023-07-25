import { useQuery, useMutation, useQueryClient } from 'react-query';
import ProjectService from '../service/project-service';

const useGetAllProjectOne = () => {
  return useQuery(['useGetAllProject'], () => ProjectService.getAllProject(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};
const useGetAllProject = () => {
  return useQuery(['useGetAllProject'], () => ProjectService.getAllProject(), {
    select: (data) =>
      data?.data?.map((project: any) => ({
        value: project.project_id,
        label: project.project_name,
      })),
  });
};

export { useGetAllProject,useGetAllProjectOne };
