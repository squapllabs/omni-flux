import { useQuery, useMutation, useQueryClient } from 'react-query';
import hscCodeService from '../service/hsnCode-service';

const useGetAllHsnCode = () => {
  return useQuery(['useGetAllHsnCode'], () => hscCodeService.getAllHsnCode(), {
    select: (data) => data.data,
  });
};

export default {
    useGetAllHsnCode
}