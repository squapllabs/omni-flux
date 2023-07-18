interface createWarehouseBody {
  warehouse_name: string;
  location: string;
  created_by: bigint;
}

interface updateWarehouseBody {
  warehouse_name: string;
  location: string;
  updated_by: bigint;
  warehouse_id: number;
}

export { createWarehouseBody, updateWarehouseBody };
