interface createHsnCodeBody {
  code: string;
  description: string;
  created_by: bigint;
}

interface updateHsnCodeBody {
  code: string;
  description: string;
  updated_by: bigint;
  hsn_code_id: number;
}

export { createHsnCodeBody, updateHsnCodeBody };
