import { useQuery,useMutation, useQueryClient } from 'react-query';
import SiteService from '../service/site-service';

const useGetAllSiteDrop = () => {
  return useQuery(['useGetAllSite'], () => SiteService.getAllSiteDrop(), {
    select: (data) =>
      data?.data?.map((site: any) => ({
        value: site.site_contractor_id,
        label: site.name,
      })),
  });
};

const createSite = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SiteService.createNewSite(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries([]);
      },
    }
  );
};

const getBySearchSiteData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SiteService.filterSiteData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const getBySiteId = (id: number) => {
  return useQuery(['getByuserID', id], () => SiteService.getOneSiteById(id), {
    select: (data) => data.data,
  });
};

const updateSite = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SiteService.updateSiteData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

export { useGetAllSiteDrop,createSite,getBySearchSiteData,getBySiteId,updateSite };