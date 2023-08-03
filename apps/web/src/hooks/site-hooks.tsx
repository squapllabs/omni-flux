import { useQuery } from 'react-query';
import SiteService from '../service/site-service';

const useGetAllSiteDrop = () => {
  return useQuery(['useGetAllSite'], () => SiteService.getAllSiteDrop(), {
    select: (data) =>
      data?.data?.map((site: any) => ({
        value: site.site_id,
        label: site.site_name,
      })),
  });
};

export { useGetAllSiteDrop };