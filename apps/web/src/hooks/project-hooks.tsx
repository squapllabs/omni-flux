import { useQuery, useMutation, useQueryClient } from 'react-query';
import ProjectService from '../service/project-service';

const useGetAllProject = () => {
  return useQuery(['useGetAllHsnCode'], () => ProjectService.getAllProject(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

export { useGetAllProject };
