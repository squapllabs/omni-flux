import { format } from 'date-fns';
import ExcelJS from 'exceljs';

const RequestForQuotationItem = async (data: any) => {

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
        'Approved Vendor',
        'Status',
        'Requested By'
    ];

    const itemHeader = [
        '',
        'S No',
        'Vendor Name',
        'Contact Person',
        'Contact Person MobNo',
        'Quotation Status',
    ]


    let headerData: any = [];
    let itemsData: any = [];
    let headerAdded = false; // Track if the header row has been added

    if (data?.length > 1) {
        data?.forEach((itemdata: any, index: number) => {

            if (!headerAdded) {
                worksheet.addRow(headers).eachCell((cell) => {
                    cell.style = headerStyle;
                });
                headerAdded = true;
            }

            headerData = [
                (index + 1).toString(),
                itemdata?.project_data?.code,
                itemdata?.project_data?.project_name,
                itemdata?.purchase_request_code,
                format(new Date(itemdata?.request_date), 'MM/dd/yyyy'),
                itemdata?.selected_vendor_data?.vendor_name || "N/A",
                itemdata?.status,
                itemdata?.requester_user_data?.first_name + " " + itemdata?.requester_user_data?.last_name
            ]


            itemsData = itemdata?.vendor_quotes?.map((data: any, index: any) => {
                return {
                    '': '',
                    'S No': (index + 1),
                    'Vendor Name': data?.vendor_data?.vendor_name,
                    'Contact Person': data?.vendor_data?.contact_person,
                    'Contact MobileNumber': data?.vendor_data?.contact_phone_no,
                    'Quotation Status': data?.quotation_status,
                }
            })

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

        })
    }
    else {
        worksheet.addRow(headers).eachCell((cell) => {
            cell.style = headerStyle;
        });

        data?.map((itemdata: any, index: number) => {
            headerData = [
                (index + 1).toString(),
                itemdata?.project_data?.code,
                itemdata?.project_data?.project_name,
                itemdata?.purchase_request_code,
                format(new Date(itemdata?.request_date), 'MM/dd/yyyy'),
                itemdata?.selected_vendor_data?.vendor_name || "N/A",
                itemdata?.status,
                itemdata?.requester_user_data?.first_name + " " + itemdata?.requester_user_data?.last_name
            ]
        })


        worksheet.addRow(itemHeader).eachCell((cell, colNumber) => {
            if (colNumber > 1) {
                cell.style = headerStyle;
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
    a.download = 'RFQ_register_item.xlsx';
    document.body.appendChild(a);
    a.click();
    window.URL.revokeObjectURL(url);
    document.body.removeChild(a);

}

export default RequestForQuotationItem