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

const useGetAllProjectStatus = () => {
  return useQuery(['useGetAllProjectStatus'], () => ProjectService.getAllProjectStatus())
}
// const useGetAllProjectDrop = () => {
//   return useQuery(
//     ['useGetAllProjectDrop'],
//     () => ProjectService.getAllProject(),
//     {
//       select: (data) =>
//         data?.data?.map((project: any) => ({
//           value: project.project_id,
//           label: project.project_name,
//         })),
//     }
//   );
// };

const useGetAllProjectDrop = () => {
  return useQuery(
    ['useGetAllProjectDrop'],
    () => ProjectService.getAllProject(),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project.project_id,
          label: project.project_name,
        })),
    }
  );
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
  return useMutation(
    (data: any) => {
      return ProjectService.filterProject(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};
const getMemberBasedProject = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.filterProjectmemberBased(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useDeleteProjects = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.deleteProject(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};

const getByProjectId = (id: number) => {
  return useQuery(
    ['getByuserID', id],
    () => ProjectService.getOneProjectById(id),
    {
      select: (data) => data.data,
    }
  );
};
const getUserDataProjectRolebased = (value: any) => {
  return useQuery(
    ['getUserDataroleAndProjectBased', value],
    () => ProjectService.getUserDataRolebasedandProjectBased(value),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project?.user_data?.user_id,
          label:
            `${project?.user_data?.first_name}` +
            ' ' +
            `${project?.user_data?.last_name}`,
        })),
    }
  );
};

const updateProject = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.updateProjectData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useGetMasterProjectParentType = () => {
  return useQuery(
    ['useGetAllmasertData'],
    () => ProjectService.getAllProjectParentType(),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project.master_data_name,
          label: project.master_data_name,
        })),
    }
  );
};

const useGetAllProjectManagers = () => {
  return useQuery(
    ['useGetAllProjectManager'],
    () => ProjectService.getAllProjectManagers(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};
export {
  useGetAllProject,
  useGetAllProjectStatus,
  createProject,
  getByProject,
  useDeleteProjects,
  getByProjectId,
  updateProject,
  useGetMasterProjectParentType,
  useGetAllProjectManagers,
  useGetAllProjectDrop,
  getMemberBasedProject,
  getUserDataProjectRolebased,
};
