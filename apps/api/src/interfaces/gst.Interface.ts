interface createGstBody {
  rate: number;
  cgst_rate: number;
  igst_rate: number;
  created_by: bigint;
}

interface updateGstBody {
  rate: number;
  cgst_rate: number;
  igst_rate: number;
  updated_by: bigint;
  gst_id: number;
}

export { createGstBody, updateGstBody };
