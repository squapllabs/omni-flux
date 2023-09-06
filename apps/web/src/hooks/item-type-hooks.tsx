import { useQuery, } from 'react-query';
import brandService from '../service/brand-service';


const useGetAllBrandForDrop = () => {
    return useQuery(
        ['useGetAllBrandDrop'],
        () => brandService.getAllBrand(),
        {
            select: (data) =>
                data?.data?.map((brand: any) => ({
                    value:brand.brand_id,
                    label:brand.brand_name,
                })),
        }
    );
};

export {
    useGetAllBrandForDrop
}