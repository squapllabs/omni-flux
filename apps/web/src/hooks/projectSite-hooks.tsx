import { useQuery, useMutation, useQueryClient } from 'react-query';
import projectSiteService from '../service/projectSite-service';

const useGetAllprojectSite = () => {
  return useQuery(
    ['useGetAllprojectSite'],
    () => projectSiteService.getAllprojectSite(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetAllprojectSiteDrop = () => {
  return useQuery(
    ['useGetAllprojectSiteDrop'],
    () => projectSiteService.getAllprojectSite(),
    {
      select: (data) =>
        data?.data?.map((projectSite: any) => ({
          value: projectSite.projectSite_id,
          label: projectSite.name,
        })),
    }
  );
};

const getByuserID = (id: number) => {
  return useQuery(
    ['getOneprojectSiteyID', id],
    () => projectSiteService.getOneprojectSiteByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.createprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteData']);
      },
    }
  );
};

const instantcreateprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.createprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteDrop']);
      },
    }
  );
};

const updateprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.updateprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteData']);
      },
    }
  );
};

const useDeleteprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.deleteprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteData']);
      },
    }
  );
};
const getByprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.filterprojectSite(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useGetAllPaginatedprojectSite = (data: any) => {
  return useQuery(
    ['useGetAllprojectSiteData'],
    () => projectSiteService.filterprojectSite(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

export {
  useGetAllprojectSite,
  getByuserID,
  createprojectSite,
  updateprojectSite,
  useDeleteprojectSite,
  useGetAllprojectSiteDrop,
  getByprojectSite,
  instantcreateprojectSite,
  useGetAllPaginatedprojectSite,
};
