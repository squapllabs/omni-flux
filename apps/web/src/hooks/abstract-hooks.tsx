import { useMutation } from 'react-query';
import BomService from '../service/bom-service';

const useCreateAbstract = () => {
  return useMutation((data: any) => {
    return BomService.createBomData(data);
  });
};

export { useCreateAbstract };
