import { format } from 'date-fns';
import ExcelJS from 'exceljs';

const RequestForQuotationRegister = async (data: any) => {

    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Sheet1');

    const headerStyle = {
        font: { color: { argb: '000000' } }, // Black font color
        fill: { type: 'pattern', pattern: 'solid', fgColor: { argb: 'd3d3d3' } },
        border: {
            top: { style: 'thin', color: { argb: '000000' } },
            bottom: { style: 'thin', color: { argb: '000000' } },
            left: { style: 'thin', color: { argb: '000000' } },
            right: { style: 'thin', color: { argb: '000000' } },
        },
    };

    const borderStyle = {
        border: {
            top: { style: 'thin', color: { argb: '000000' } },
            bottom: { style: 'thin', color: { argb: '000000' } },
            left: { style: 'thin', color: { argb: '000000' } },
            right: { style: 'thin', color: { argb: '000000' } },
        },
    };

    const headers = [
        'S No',
        'Project Code',
        'Project Name',
        'RFQ Number',
        'RFQ Date',
        'Status',
        'Requested By'
    ];

    const itemHeader = [
        '',
        'S No',
        'Item Name',
        'UOM',
        // 'RFQ Quantity',
        // 'Rate',
        // 'Total Amount',
    ]

    let headerData: any = [];
    let itemsData: any = [];

    let headerAdded = false; // Track if the header row has been added

    if (data?.length > 1) {
        data?.forEach((itemData: any, index: number) => {
            if (!headerAdded) {
                worksheet.addRow(headers).eachCell((cell) => {
                    cell.style = headerStyle;
                });
                headerAdded = true;
            }

            // if (itemData?.purchase_order_type === "Head Office") {
            headerData = [
                (index + 1),
                itemData?.project_data?.code,
                itemData?.project_data?.project_name,
                itemData?.purchase_request_code,
                format(new Date(itemData?.request_date), 'MM/dd/yyyy'),
                itemData?.status,
                itemData?.requester_user_data?.first_name + " " + itemData?.requester_user_data?.last_name,

            ];

            if (itemData?.vendor_quotes && itemData?.vendor_quotes.length > 0) {
                itemsData = itemData?.vendor_quotes[0]?.vendor_quotation_details?.map((item: any, index: number) => {
                    return {
                        '': '',
                        'S No': (index + 1),
                        'Item Name': item?.item_data?.item_name,
                        'UOM': item?.item_data?.uom?.name,
                    }
                })
            }

            worksheet.addRow(headerData).eachCell((cell) => {
                cell.style = borderStyle;
            });

            if (itemsData && itemsData?.length > 0) {
                worksheet.addRow(itemHeader).eachCell((cell, colNumber) => {
                    if (colNumber > 1) {
                        cell.style = headerStyle;
                    }
                });
            }

            itemsData?.forEach((item: any) => {
                const rowData = Object.values(item);
                worksheet.addRow(rowData).eachCell((cell, colNumber) => {
                    if (colNumber > 1) { // Skip the first column (A3)
                        cell.style = borderStyle;
                    }
                });
            });

        });

    }
    else {

        worksheet.addRow(headers).eachCell((cell) => {
            cell.style = headerStyle;
        });

        data?.map((headerData: any, index: number) => {
            const rowData = [
                (index + 1),
                headerData?.purchase_request_data?.project_data?.code,
                headerData?.purchase_request_data?.project_data?.project_name,
                headerData?.purchase_request_data?.purchase_request_code,
                format(new Date(headerData?.purchase_request_data?.request_date), 'MM/dd/yyyy'),
                headerData?.purchase_request_data?.status,
                headerData?.purchase_request_data?.requester_user_data?.first_name + " " + headerData?.purchase_request_data?.requester_user_data?.last_name,
            ];
            headerData.push(rowData);
        })

        itemsData = data?.vendor_quotes[0]?.vendor_quotation_details?.map((item: any, index: number) => {
            return {
                '': '',
                'S No': (index + 1),
                'Item Name': item?.item_data?.item_name,
                'UOM': item?.item_data?.uom?.name,
            }
        })

        worksheet.addRow(headerData).eachCell((cell) => {
            cell.style = borderStyle;
        });

        worksheet.addRow(itemsData).eachCell((cell, colNumber) => {
            if (colNumber > 1) { // Skip the first column (A3)
                cell.style = borderStyle;
            }
        });

    }
    worksheet.columns.forEach((column) => {
        let maxLength = 0;
        column.eachCell({ includeEmpty: true }, (cell) => {
            const length = cell.value ? cell.value.toString().length : 10;
            if (length > maxLength) {
                maxLength = length;
            }
        });
        column.width = maxLength < 10 ? 10 : maxLength + 2; // Add some padding
    });
    // Generate a buffer containing the Excel file
    const buffer = await workbook.xlsx.writeBuffer();

    // Create a Blob from the buffer
    const blob = new Blob([buffer], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });

    // Create a download link and trigger the download
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'RFQ_register.xlsx';
    document.body.appendChild(a);
    a.click();
    window.URL.revokeObjectURL(url);
    document.body.removeChild(a);
}

export default RequestForQuotationRegister;
