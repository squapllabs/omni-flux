import React, { useState } from 'react';
import jsPDF from 'jspdf';
import 'jspdf-autotable';
import { Document, Page } from 'react-pdf';
import { format } from 'date-fns';
import { formatBudgetValue } from '../../helper/common-function';

const ReportGenerator = (data: any) => {

    const itemsData = data?.purchase_request_data?.purchase_request_details?.map((item: any) => ({
        itemName: item?.item_name,
        quantity: item?.purchase_requested_quantity,
        unitPrice: item?.rate,
        total: item?.purchase_requested_quantity * item?.rate
    }))

    // console.log("data?.purchase_request_data?.purchase_request_details",data?.purchase_request_data?.purchase_request_details);
    
    // const itemsData = [];

    // for (let i = 0; i < 150; i++) {
    //     itemsData.push({
    //         itemName: 'Water Tanks',
    //         quantity: 22,
    //         unitPrice: 1000,
    //         total: 22 * 1000, // Calculate the total for each item
    //     });
    // }

    const overallTotal = itemsData.reduce((accumulator: any, currentItem: any) => {
        return accumulator + currentItem.total;
    }, 0);
    const overallTotalQuantity = itemsData.reduce((accumulator: any, currentItem: any) => {
        return accumulator + currentItem.quantity;
    }, 0);

    const purchaseOrder = {
        companyName: 'Eco Protection Engineers',
        companyAddress: 'Plot No, 943, 54th St',
        Line1: 'TVS Colony,Anna Nagar West Extension',
        phoneNumber: '044 2654 5180',
        emailAddress: 'mail@ecoprotection.in',
        websiteURL: 'https://ecoprotection.in/',
        orderNumber: data?.order_id,
        orderDate: format(
            new Date(data?.order_date),
            'MMM dd, yyyy'
        ),
        vendorName: data?.vendor_data?.vendor_name,
        vendorAddress: Object.values(data?.vendor_data?.address),
        vendorContact: data?.vendor_data?.contact_phone_no,
        siteName: data?.purchase_request_data?.site_data?.name,
        siteAddress: Object.values(data?.purchase_request_data?.site_data?.address),
        siteContact: data?.purchase_request_data?.site_data?.mobile_number,
        subtotal: data?.purchase_request_data.total_cost,
        // taxRate: 8, // 8% tax rate
    };

    // Create a new jsPDF instance
    const pdf = new jsPDF();
    pdf.setProperties({
        title: "purchase order"
    })

    // Add content to the PDF
    const yStart = 15;
    const yOffset = 10;

    pdf.setFont('custom');

    const imageUrl = "/Ecologo-03.png"; // Replace with your image URL
    pdf.addImage(imageUrl, 'JPEG', 10, 1, 55, 15); //text to added in list of particals
    // Title
    pdf.setFontSize(15);
    pdf.setFont('Newsreader', 'bold');
    pdf.text('PURCHASE ORDER', 85, 10);
    pdf.setFontSize(11);
    pdf.setFont('Newsreader', 'normal');
    pdf.text(`${purchaseOrder.companyAddress}`, 14, 25);
    pdf.text(`${purchaseOrder.Line1}`, 14, 30);
    pdf.text(`${purchaseOrder.phoneNumber}`, 14, 35);
    pdf.text(`${purchaseOrder.emailAddress}`, 14, 40);
    pdf.text(`${purchaseOrder.websiteURL}`, 14, 45);
    pdf.setFontSize(12);
    pdf.text(`Order Number: ${purchaseOrder.orderNumber}`, 140, 30);
    pdf.text(`Order Date: ${purchaseOrder.orderDate}`, 140, 35);

    const headerStyles = {
        // fillColor: [0, 0, 255],
        fillColor: [0, 51, 102],
        textColor: 255,
        fontStyle: 'bold',
    };

    const vendorDetailsTable = {
        head: [['Vendor Details']],
        body: [
            [`Vendor Name: ${purchaseOrder.vendorName}`],
            [`Vendor Address: ${purchaseOrder.vendorAddress}`],
            [`Contact Number: ${purchaseOrder.vendorContact}`]
        ],
        startX:10,
        startY: 50, // Adjust the Y position as needed
        headStyles: headerStyles,
    };
    pdf.autoTable(vendorDetailsTable);

    // Create a table for Site / Shipping Details
    const siteDetailsTable = {
        head: [['Site / Shipping Details']],
        body: [
            [`Site Name: ${purchaseOrder.siteName}`],
            [`Site Address: ${purchaseOrder.siteAddress}`],
            [`Contact Number: ${purchaseOrder.siteContact}`]
        ],
        startY: 85, // Adjust the Y position as needed
        headStyles: headerStyles,
    };
    pdf.autoTable(siteDetailsTable);

    // Define item details table headers
    const itemDetailsHeaders = ['S.No', 'Item Name', 'Quantity', 'Unit Price', 'Total'];
    pdf.setFont('courier');
    const itemDetailsRows = itemsData?.map((item: any, index: number) => [
        index + 1,
        item.itemName,
        item.quantity?.toString(),
        item.unitPrice?.toString(),
        item.total?.toLocaleString(),
    ]);



    // Create item details table
    const itemDetailsYStart = yStart + yOffset * 10.5;
    pdf.autoTable({
        head: [itemDetailsHeaders],
        body: itemDetailsRows,
        startY: itemDetailsYStart, // Adjust the Y position as needed
        headStyles: headerStyles,
    });

    // Define summary table rows ₹ 
    const summaryRows = [
        [{ content: 'Total Quantity:', styles: { fontStyle: 'bold' } }, overallTotalQuantity.toLocaleString()],
        [{ content: 'Total Amount:', styles: { fontStyle: 'bold' } }, overallTotal.toLocaleString()],
    ];

    // const totalPages = pdf.internal.getNumberOfPages();
    const totalPages = pdf.internal.getNumberOfPages();
    for (let i = 1; i <= totalPages; i++) {
        pdf.setPage(i);
        pdf.setFont('Newsreader');
        pdf.text(
            `Page ${i} of ${totalPages}`,
            185,
            pdf.internal.pageSize.getHeight() - 5
        );
    }



    // Create summary table
    const summaryYStart = itemDetailsYStart + pdf.autoTable.previous;

    pdf.autoTable({
        body: summaryRows,
        startY: summaryYStart, // Adjust the Y position as needed
    });

    // Save the PDF
    pdf.save('purchase_order.pdf');

    // const pdfDataUri = pdf.output("datauristring");
    // const previewWindow = window.open();
    // previewWindow?.document.write(
    //     "<iframe width='100%' height='100%' src='" + pdfDataUri + "'></iframe>"
    // );

};

export default ReportGenerator;