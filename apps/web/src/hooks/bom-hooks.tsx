import { useQuery, useMutation, useQueryClient } from 'react-query';
import BomService from '../service/bom-service';

const useCreateBom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return BomService.createBomData(data);
    },
    {
      onSuccess: (response, _var) => {
        queryClient.invalidateQueries([
          'getOneSubcategoryID',
          _var[0].sub_category_id,
        ]);
      },
    }
  );
};
const useCreateBulkBom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return BomService.createBulkBom(data);
    },
    {
      onSuccess: (response) => {
        queryClient.invalidateQueries([
          'getBOMDetails',
          {
            projectId: response?.data?.bom_configuration_details?.project_id,
            boQId:
              response?.data?.bom_configuration_details?.bom_configuration_id,
          },
        ]);
      },
    }
  );
};

const useGetBOMbyProjectandType = (value: any) => {
  return useQuery(
    ['getBOMbyProjectandType', value],
    () => BomService.getBOMbyProjectandType(value),
    {
      select: (data) =>
        data?.data?.map((option: any) => ({
          value: option.bom_detail_id,
          label: option.item_data?.item_name,
          item_rate: option.item_data?.rate,
          bom_rate: option.rate,
          bom_quantity: option.quantity,
          temp: option,
        })),
    }
  );
};

const useCreateBoQ = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return BomService.addBoQ(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllProjectBOQData']);
      },
    }
  );
};

const useGetBySearchBoQProject = (data: any) => {
  return useQuery(
    ['useGetAllProjectBOQData'],
    () => BomService.getProjectBoQList(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useUpdateBoQ = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return BomService.updateBoQ(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllProjectBOQData']);
      },
    }
  );
};

// const useDeleteProjectMember = () => {
//   const queryClient = useQueryClient();
//   return useMutation(
//     (data: any) => {
//       return BomService.deleteProjectMember(data);
//     },
//     {
//       onSuccess: () => {
//         queryClient.invalidateQueries(['useGetAllProjectPaginatedData']);
//       },
//     }
//   );
// };

// const useGetAllPaginatedProjectMember = (data: any) => {
//   return useQuery(
//     ['useGetAllProjectPaginatedData'],
//     () => BomService.filterProjectMember(data),
//     {
//       select: (data) => data,
//       staleTime: Infinity,
//     }
//   );
// };

export {
  useCreateBom,
  useCreateBulkBom,
  useGetBOMbyProjectandType,
  useCreateBoQ,
  useGetBySearchBoQProject,
  useUpdateBoQ,
};
