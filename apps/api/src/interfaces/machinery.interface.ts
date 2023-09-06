interface machineryBody {
    machinery_id: number;
    machinery_name: string;
    machinery_model: string;
    machinery_type: string;
    manufacturer: string;
    date_of_purchase: Date;
    warranty_expired_on: Date;
    operational_status: string;
    location: string;
    rate: number;
    uom_id: number;
    is_delete: boolean;
    created_by: number;
    updated_by: number;
}

export { machineryBody };
