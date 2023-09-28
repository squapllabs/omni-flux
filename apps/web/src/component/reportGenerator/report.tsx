import React, { useState } from 'react';
import jsPDF from 'jspdf';
import 'jspdf-autotable';
import { Document, Page } from 'react-pdf';

const ReportGenerator = (props: any) => {
    const [numPages, setNumPages] = useState<number | null>(null);
    const [pageNumber, setPageNumber] = useState(1);
    const [pdfUrl, setPdfUrl] = useState<string | null>(null);

    const onDocumentLoadSuccess = ({ numPages }: { numPages: number }) => {
        setNumPages(numPages);
    };

    const generatePDF = () => {
        // Your invoice data here...
        const purchaseOrder = {
            companyName: 'Eco Protection Engineers',
            companyAddress: 'Plot No, 943, 54th St',
            Line1: 'TVS Colony,Anna Nagar West Extension',
            phoneNumber: '044 2654 5180',
            emailAddress: 'mail@ecoprotection.in',
            websiteURL: 'https://ecoprotection.in/',
            orderNumber: 'PO-12345',
            orderDate: '08-08-2023',
            vendorName: 'India cements',
            vendorAddress: '13,anna salai,chennai - 23 ',
            vendorContact: '9877567212',
            siteName: 'Anna Nagar Easte',
            siteAddress: '14/203 , J Block , Anna Nagar Easte',
            siteContact: '9442838007',
            items: [
                {
                    itemName: 'Product A',
                    itemDescription: 'Product A Description',
                    quantity: 5,
                    unitPrice: 50,
                    total: 250,
                },
                {
                    itemName: 'Product B',
                    itemDescription: 'Product B Description',
                    quantity: 3,
                    unitPrice: 40,
                    total: 120,
                },
                // Add more items as needed
            ],
            subtotal: 370,
            taxRate: 8, // 8% tax rate
        };

        // Create a new jsPDF instance
        const pdf = new jsPDF();

        // Set the font to monospaced for consistent spacing


        // Add content to the PDF
        const yStart = 15;
        const yOffset = 10;

        pdf.setFont('verdana');
        // Company Name
        // pdf.setFontSize(12);

        const imageUrl = "/Ecologo-03.png"; // Replace with your image URL
        pdf.addImage(imageUrl, 'JPEG', 10, 1, 55, 15, 'center'); //text to added in list of particals

        // Title
        pdf.setFontSize(15);
        // pdf.text(`${purchaseOrder.companyName}`, 105, yStart + yOffset);
        pdf.text('PURCHASE ORDER', 140, yStart + yOffset * 1);
        pdf.setFontSize(11);
        pdf.setLineHeightFactor(0.1);
        pdf.text(`${purchaseOrder.companyAddress}`, 10, yStart + yOffset * 1 + 2);
        pdf.text(`${purchaseOrder.Line1}`, 10, yStart + yOffset * 2 + 2 * 0.1);
        pdf.text(`${purchaseOrder.phoneNumber}`, 10, yStart + yOffset * 3 + 2 * 0.2);
        pdf.text(`${purchaseOrder.emailAddress}`, 10, yStart + yOffset * 4 + 2 * 0.3);
        pdf.text(`${purchaseOrder.websiteURL}`, 10, yStart + yOffset * 5 + 2 * 0.4);
        pdf.setFontSize(12);
        pdf.text(`Order Number: ${purchaseOrder.orderNumber}`, 140, yStart + yOffset * 2);
        pdf.text(`Order Date: ${purchaseOrder.orderDate}`, 140, yStart + yOffset * 3);
        pdf.text('Vendor Details', 105, yStart + yOffset * 6, 'center');
        pdf.text(`Vendor Name: ${purchaseOrder.vendorName}`, 10, yStart + yOffset * 7);
        pdf.text(`Vendor Address: ${purchaseOrder.vendorAddress}`, 10, yStart + yOffset * 8);
        pdf.text(`Vendor Contact: ${purchaseOrder.vendorContact}`, 105, yStart + yOffset * 8);
        pdf.text('Site Details / Shipping Details', 105, yStart + yOffset * 9, 'center');
        pdf.text(`Site Name: ${purchaseOrder.siteName}`, 10, yStart + yOffset * 10);
        pdf.text(`Site Address: ${purchaseOrder.siteAddress}`, 10, yStart + yOffset * 11);
        pdf.text(`Contact Number: ${purchaseOrder.siteContact}`, 105, yStart + yOffset * 11);

        // Addresses
        // pdf.text(`Billing Address: ${purchaseOrder.billingAddress}`, 10, yStart + yOffset * 8);
        // pdf.text(`Shipping Address: ${purchaseOrder.shippingAddress}`, 10, yStart + yOffset * 9);

        // Payment Terms
        // pdf.text(`Payment Terms: ${purchaseOrder.paymentTerms}`, 10, yStart + yOffset * 10);

        // Define item details table headers
        const itemDetailsHeaders = ['Item Name', 'Description', 'Quantity', 'Unit Price', 'Total'];
        pdf.setFont('courier');
        // Define item details table rows
        const itemDetailsRows = purchaseOrder.items.map((item) => [
            item.itemName,
            item.itemDescription,
            item.quantity.toString(),
            item.unitPrice.toString(),
            item.total.toString(),
        ]);

        // Create item details table
        const itemDetailsYStart = yStart + yOffset * 12;
        pdf.autoTable({
            head: [itemDetailsHeaders],
            body: itemDetailsRows,
            startY: itemDetailsYStart, // Adjust the Y position as needed
        });

        // Calculate tax amount
        const taxAmount = (purchaseOrder.taxRate / 100) * purchaseOrder.subtotal;

        // Calculate total amount
        const totalAmount = purchaseOrder.subtotal + taxAmount;

        // Define summary table rows
        const summaryRows = [
            ['Subtotal:', purchaseOrder.subtotal.toString()],
            [`Tax (${purchaseOrder.taxRate}%):`, taxAmount.toString()],
            ['Total:', totalAmount.toString()],
        ];

        // Create summary table
        const summaryYStart = itemDetailsYStart + pdf.autoTable.previous.finalY + 10;
        pdf.autoTable({
            body: summaryRows,
            startY: summaryYStart, // Adjust the Y position as needed
        });

        // Save the PDF
        pdf.save('invoice.pdf');

        // const pdfBlob = pdf.output('blob');
        // const pdfUrl = URL.createObjectURL(pdfBlob);

        // Set the PDF URL for preview
        // setPdfUrl(pdfUrl);
    };

    return (
        <div>
            <h1>Hello, React with jsPDF!</h1>
            <p>Click the button below to generate a PDF invoice.</p>
            <button onClick={generatePDF}>Generate PDF</button>
        </div>
        // <div>
        //     <h1>Hello, React with jsPDF!</h1>
        //     <p>Click the button below to generate and preview a PDF purchase order.</p>
        //     <button onClick={generatePDF}>Generate PDF</button>
        //     {pdfUrl && (
        //         <div>
        //             <Document file={pdfUrl} onLoadSuccess={onDocumentLoadSuccess}>
        //                 <Page pageNumber={pageNumber} />
        //             </Document>
        //             <p>
        //                 Page {pageNumber} of {numPages}
        //             </p>
        //             <button onClick={() => setPageNumber(pageNumber - 1)}>Previous Page</button>
        //             <button onClick={() => setPageNumber(pageNumber + 1)}>Next Page</button>
        //         </div>
        //     )}
        // </div>

    );
};

export default ReportGenerator;
