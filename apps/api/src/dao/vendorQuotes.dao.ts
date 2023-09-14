import prisma from '../utils/prisma';

const add = async (
    vendor_id: number,
    purchase_request_id: number,
    quotation_date: Date,
    quotation_status: string,
    total_quotation_amount: number,
    remarks: string,
    quotation_details: JSON,
    created_by: number,
    connectionObj = null
) => {
    try {
        const currentDate = new Date();
        const is_delete = false;
        const transaction = connectionObj !== null ? connectionObj : prisma;
        const vendorQuotes = await transaction.vendor_quotes.create({
            data: {
                vendor_id,
                purchase_request_id,
                quotation_date,
                quotation_status,
                total_quotation_amount,
                remarks,
                quotation_details,
                created_by,
                created_date: currentDate,
                updated_date: currentDate,
                is_delete: is_delete,
            },
        });
        return vendorQuotes;
    } catch (error) {
        console.log('Error occurred in vendorQuotesDao add', error);
        throw error;
    }
};

const edit = async (
    vendor_quotes_id: number,
    vendor_id: number,
    purchase_request_id: number,
    quotation_date: Date,
    quotation_status: string,
    total_quotation_amount: number,
    remarks: string,
    quotation_details: JSON,
    updated_by: number,
    connectionObj = null
) => {
    try {
        const currentDate = new Date();
        const transaction = connectionObj !== null ? connectionObj : prisma;
        const vendorQuotes = await transaction.vendor_quotes.update({
            where: {
                vendor_quotes_id: Number(vendor_quotes_id),
            },
            data: {
                vendor_id,
                purchase_request_id,
                quotation_date,
                quotation_status,
                total_quotation_amount,
                remarks,
                quotation_details,
                updated_by,
                updated_date: currentDate,
            },
        });
        return vendorQuotes;
    } catch (error) {
        console.log('Error occurred in vendorQuotesDao edit', error);
        throw error;
    }
};

const getById = async (vendorQuotesId: number, connectionObj = null) => {
    try {
        const transaction = connectionObj !== null ? connectionObj : prisma;
        const vendorQuotes = await transaction.vendor_quotes.findFirst({
            where: {
                vendor_quotes_id: Number(vendorQuotesId),
                is_delete: false,
            },
        });
        return vendorQuotes;
    } catch (error) {
        console.log('Error occurred in vendorQuotes getById dao', error);
        throw error;
    }
};

const getAll = async (connectionObj = null) => {
    try {
        const transaction = connectionObj !== null ? connectionObj : prisma;
        const vendorQuotes = await transaction.vendor_quotes.findMany({
            where: {
                is_delete: false,
            },
            orderBy: [
                {
                    updated_date: 'desc',
                },
            ],
        });
        return vendorQuotes;
    } catch (error) {
        console.log('Error occurred in vendorQuotes getAll dao', error);
        throw error;
    }
};

const deletevendorQuotes = async (
    vendorQuotesId: number,
    connectionObj = null
) => {
    try {
        const transaction = connectionObj !== null ? connectionObj : prisma;
        const vendorQuotes = await transaction.vendor_quotes.update({
            where: {
                vendor_quotes_id: Number(vendorQuotesId),
            },
            data: {
                is_delete: true,
            },
        });
        return vendorQuotes;
    } catch (error) {
        console.log(
            'Error occurred in vendorQuotes deletevendorQuotes dao',
            error
        );
        throw error;
    }
};

const searchvendorQuotes = async (
    offset: number,
    limit: number,
    orderByColumn: string,
    orderByDirection: string,
    global_search,
    status,
    connectionObj = null
) => {
    try {
        const transaction = connectionObj !== null ? connectionObj : prisma;
        const globalSearch = global_search.toLowerCase();
        const order_by_column = orderByColumn;
        const order_by_direction = orderByDirection;
        const is_delete = status === 'AC' ? false : true;

        const getAllVendorQuotes = await transaction.$queryRawUnsafe(
            `SELECT *,
                CAST(created_by AS int) AS created_by,
                CAST(updated_by AS int) AS updated_by
                FROM vendor_quotes
            WHERE
                (
                    remarks ILIKE '%' || '${globalSearch}' || '%' OR
                    quotation_status ILIKE '%' || '${globalSearch}' || '%' OR
                    quotation_details->>'item_name' ILIKE '%' || '${globalSearch}' || '%'
                )
                AND (is_delete = ${is_delete})
                ORDER BY ${order_by_column} ${order_by_direction}`
        );


        const vendorQuotesCount = getAllVendorQuotes.length;
        const pagedVendorQuotes = getAllVendorQuotes.slice(
            offset,
            offset + limit
        );
        const vendorQuotesData = {
            count: vendorQuotesCount,
            data: pagedVendorQuotes,
        };
        return vendorQuotesData;
    } catch (error) {
        console.log(
            'Error occurred in vendorQuotes dao : searchvendorQuotes',
            error
        );
        throw error;
    }
};

export default {
    add,
    edit,
    getById,
    getAll,
    deletevendorQuotes,
    searchvendorQuotes,
};
