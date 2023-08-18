interface createRoleBody {
  role_name: string;
  created_by: bigint;
  description: string;
}

interface updateRoleBody {
  role_name: string;
  updated_by: bigint;
  role_id: number;
  description: string;
}

export { createRoleBody, updateRoleBody };
