interface createBrandBody{
    brand_id:number;
    brand_name: string;
    created_by:bigint;
    updated_by:bigint;
}
interface updateBrandBody{
    brand_id:number;
    brand_name: string;
    updated_by:bigint;
}

export {createBrandBody,updateBrandBody}