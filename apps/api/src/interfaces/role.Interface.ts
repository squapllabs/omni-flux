interface createRoleBody {
  role_name: string;
  created_by: bigint;
}

interface updateRoleBody {
  role_name: string;
  updated_by: bigint;
  role_id: number;
}

export { createRoleBody, updateRoleBody };
