interface createMasterDataBody {
  master_data_name: string;
  master_data_description: string;
  master_data_type: string;
  parent_master_data_id: number;
  created_by: bigint;
}

interface updateMasterDataBody {
  master_data_id: number;
  master_data_name: string;
  master_data_description: string;
  master_data_type: string;
  parent_master_data_id: number;
  updated_by: bigint;
}

export { createMasterDataBody, updateMasterDataBody };
