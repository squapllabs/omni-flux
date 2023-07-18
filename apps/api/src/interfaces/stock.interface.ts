interface createStockBody{
    product_id: number;
    transaction_type: string;
    quantity: number;
    transaction_date:string;
    warehouse_id:number;
    site_id:number;
    created_by:bigint;
    updated_by:bigint;
  }
interface updateStockBody{
    stock_id:number;
    product_id: number;
    transaction_type: string;
    quantity: number;
    transaction_date:string;
    warehouse_id:number;
    site_id:number;
    updated_by:bigint;
  }
  export {
    createStockBody,
    updateStockBody
  }