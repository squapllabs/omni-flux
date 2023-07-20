interface createWarehouseInventoryBody {
  warehouse_id: number;
  product_id: number;
  quantity: number;
  created_by: bigint;
  item_id: number;
}

interface updateWarehouseInventoryBody {
  warehouse_id: number;
  product_id: number;
  quantity: number;
  updated_by: bigint;
  item_id: number;
  warehouse_inventory_id: number;
}

export { createWarehouseInventoryBody, updateWarehouseInventoryBody };
