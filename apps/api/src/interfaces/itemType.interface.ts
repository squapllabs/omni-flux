interface createItemTypeBody{
    item_type_id:number;
    item_type_item_code:string;
    item_type_item_name: string;
    created_by:bigint;
    updated_by:bigint;
}
interface updateItemTypeBody{
    item_type_id:number;
    item_type_item_code:string;
    item_type_item_name: string;
    updated_by:bigint;
}

export {createItemTypeBody,updateItemTypeBody}