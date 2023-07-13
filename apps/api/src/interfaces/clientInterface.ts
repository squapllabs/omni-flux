interface createClientBody {
  name: string;
  contact_details: string;
  created_by: bigint;
}

interface updateClientBody {
  client_id: number;
  name: string;
  contact_details: string;
  updated_by: bigint;
}

export { createClientBody, updateClientBody };
