import React from 'react'
import { format } from 'date-fns';
import ExcelJS from 'exceljs';

const PurchaseRegister = async (data: any) => {
    // console.log("dataaaaaa", data);


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
    }

    const headers = [
        'S No',
        'Purchase Type',
        'Project Code',
        'Project Name',
        'Order Number',
        'Order Date',
        'Vendor Name',
        'Total Amount',
        'Requested By'
    ];

    // if (data.some(item => item?.purchase_order_type === "Head Office")) {
    //     // Include 'Vendor Name' header only if there is a "Head Office" purchase order type
    //     headers.splice(6, 0, 'Vendor Name');
    // }


    let headerData: any = [];
    if (data) {
        headerData = data?.map((item: any, rowIndex: number) => {
            if (item?.purchase_order_type === "Head Office") {
                return {
                    'Sl No.': (rowIndex + 1).toString(),
                    'Purchase Type': item?.purchase_order_type,
                    'Project Code': item?.purchase_request_data?.project_data?.code,
                    'Project Name': item?.purchase_request_data?.project_data?.project_name,
                    'Order Number': item?.order_id,
                    'Order Date': format(new Date(item?.order_date), 'MM/dd/yyyy'),
                    'Vendor Name': item?.purchase_request_data?.selected_vendor_data?.vendor_name,
                    'Total Amount': item?.purchase_request_data?.total_cost,
                    'Requested By': item?.purchase_request_data?.requester_user_data?.first_name + " " + item?.purchase_request_data?.requester_user_data?.last_name
                };
            }
            else {
                return {
                    'Sl No.': (rowIndex + 1).toString(),
                    'Purchase Type': item?.purchase_order_type,
                    'Project Code': item?.indent_request_data?.project_data?.code,
                    'Project Name': item?.indent_request_data?.project_data?.project_name,
                    'Order Number': item?.order_id,
                    'Order Date': format(new Date(item?.order_date), 'MM/dd/yyyy'),
                    'Vendor Name': item?.purchase_request_data?.selected_vendor_data?.vendor_name || "N/A",
                    'Total Amount': item?.indent_request_data?.total_cost,
                    'Requested By': item?.indent_request_data?.requester_user_data?.first_name + " " + item?.indent_request_data?.requester_user_data?.last_name
                };
            }
        });
    }


    worksheet.addRow(headers).eachCell((cell) => {
        cell.style = headerStyle;
    });

    headerData?.forEach((item: any) => {
        const rowData = Object.values(item);
        worksheet.addRow(rowData).eachCell((cell, colNumber) => {
            // if (colNumber > 1) { // Skip the first column (A3)
            cell.style = borderStyle;
            // }
        });
    });

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
    a.download = 'purchase_register.xlsx';
    document.body.appendChild(a);
    a.click();
    window.URL.revokeObjectURL(url);
    document.body.removeChild(a);
}

export default PurchaseRegister