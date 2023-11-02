import React from 'react'
import jsPDF from 'jspdf';
import 'jspdf-autotable';
import { format } from 'date-fns';

const RequestForQuotation = (data: any) => {

    const itemsData = data?.vendor_quotes[0]?.vendor_quotation_details?.map((item: any) => ({
        itemName: item?.item_data?.item_name,
        quantity: item?.purchase_requested_quantity || "N/A",
        uom: item?.item_data?.uom?.name || "N/A",
        unitPrice: item?.item_data?.rate || "N/A",
        total: item?.purchase_requested_quantity * item?.item_data?.rate || "N/A",
    }))

    console.log("item", itemsData);


    const vendorData = data?.status === "Approved" ? [{
        vendorName: data?.selected_vendor_data?.vendor_name,
        vendorAddress: `${data?.selected_vendor_data?.address?.street || ''} ${data?.selected_vendor_data?.address?.city || ''} ${data?.selected_vendor_data?.address?.state || ''} ${data?.selected_vendor_data?.address?.country || ''}`,
        vendorPinCode: data?.selected_vendor_data?.address?.pin_code || '',
        contactPerson: data?.selected_vendor_data?.contact_person || '',
        contactPersonMobNo: data?.selected_vendor_data?.contact_phone_no || '',
    }] : data?.vendor_quotes?.map((vendor: any, index: number) => ({
        vendorName: vendor?.vendor_data?.vendor_name,
        vendorAddress: `${vendor?.vendor_data?.address?.street || ''} ${vendor?.vendor_data?.address?.city || ''} ${vendor?.vendor_data?.address?.state || ''} ${vendor?.vendor_data?.address?.country || ''}`,
        vendorPinCode: vendor?.vendor_data?.address?.pin_code || '',
        contactPerson: vendor?.vendor_data?.contact_person || '',
        contactPersonMobNo: vendor?.vendor_data?.contact_phone_no || '',
    }))

    console.log("vendorData", vendorData);

    // const itemsData = [];

    // for (let i = 0; i < 150; i++) {
    //     itemsData.push({
    //         itemName: 'Water Tanks',
    //         quantity: "22",
    //         uom: "N/A",
    //         unitPrice: "1000",
    //         total: "0", // Calculate the total for each item
    //     });
    // }
    const pdf = new jsPDF();
    pdf.setProperties({
        title: "Request For Quotation"
    })
    const callImage = "/Calling.png";

    const generateCommonContent = (pdf: any) => {
        const imageUrl = "/Ecologo-03.png"; // Replace with your image URL
        pdf.addImage(imageUrl, 'JPEG', 10, 5, 40, 10); //text to added in list of particals
        pdf.setFontSize(10);
        pdf.setFont('custom', 'bold');
        pdf.text('REQUEST FOR QUOTATION', 150, 12);
        pdf.setLineWidth(0.1); // Line width in units (you can adjust this)
        pdf.setDrawColor(200, 200, 200); // Line color (RGB)
        pdf.line(10, 18, 200, 18)
        pdf.text('Contact Person', 13, 23)
        pdf.setFont('custom', 'normal');
        pdf.text(`${data?.requester_user_data?.first_name + " " + data?.requester_user_data?.last_name}`, 13, 27)
        pdf.addImage(callImage, 'PNG', 13, 28, 3, 3);
        pdf.text(`  ${data?.requester_user_data?.contact_no}`, 16, 31)
        pdf.setFont('Newsreader', 'bold')
        pdf.text('RFQ No      :', 130, 23)
        pdf.text('RFQ Date   :', 130, 27)
        pdf.text('Due Date    :', 130, 31)
        pdf.setFont('Newsreader', 'normal')
        pdf.text(`${data?.purchase_request_code}`, 155, 23)
        pdf.text(format(new Date(data?.request_date), 'MMM dd, yyyy'), 155, 27)
        pdf.text(format(new Date(data?.indent_request_data?.expected_delivery_date), 'MMM dd, yyyy'), 155, 31)
        pdf.line(10, 34, 200, 34)
        pdf.setFont('Newsreader', 'bold')
        pdf.text('To', 13, 39)
        pdf.setFont('Newsreader', 'bold')
        pdf.text('Purchase Centre Address :', 130, 39)
        pdf.setFont('Newsreader', 'normal')
        pdf.text('Head Office', 130, 44)
        pdf.text('CHENNAI', 130, 48)
        pdf.setFont('Newsreader', 'bold')
        pdf.text('Dear Sir,', 13, 70)
        pdf.setFont('Newsreader', 'normal')
        pdf.text('Please send your most competitive offer/mentioning your Terms & Conditions before the due date. You can send the same to \nthe above mentioned e-mail/fax', 13, 77)
        pdf.setFont('Newsreader', 'normal')
        pdf.setFontSize(10);

        const itemDetailsRows = itemsData?.map((item: any, index: number) => [
            (index + 1).toString(),
            item.itemName.toString(),
            item.quantity?.toString(),
            item.uom?.toString(),
            // item.unitPrice?.toString(),
            item.total?.toLocaleString(),
        ]);
        const itemDetailsHeaders = ['S.No', 'Item Name', 'Quantity', 'UOM', 'Total'];
        const columnWidths = [15, 90, 30, 30, 23]; // Adjust column widths as needed
        // Define table styles
        const headerStyles = {
            fillColor: [240, 240, 240],
            textColor: [0],
            fontFamily: 'Newsreader',
            fontStyle: 'bold',
        };


        pdf.setFont('Newsreader');
        const itemDetailsYStart = 86;
        pdf.autoTable({
            head: [itemDetailsHeaders],
            body: itemDetailsRows,
            startY: itemDetailsYStart, // Adjust the Y position as needed
            headStyles: {
                fillColor: headerStyles.fillColor,
                textColor: headerStyles.textColor,
                fontStyle: headerStyles.fontStyle,
                fontSize: 10, // Adjust the font size as needed
                font: 'Newsreader', // Set the font family
                halign: 'left',
            },
            columnStyles: {
                0: { cellWidth: columnWidths[0] }, // Adjust column widths as needed
                1: { cellWidth: columnWidths[1] },
                2: { cellWidth: columnWidths[2] },
                3: { cellWidth: columnWidths[3] },
                4: { cellWidth: columnWidths[4] },
                // 5: { cellWidth: columnWidths[5] },
            },
            alternateRowStyles: { fillColor: [255, 255, 255] },
            bodyStyles: {
                fontSize: 10, // Adjust the font size for the body
                font: 'Newsreader', // Set the font family for the body
                cellPadding: { top: 1, right: 5, bottom: 1, left: 2 }, // Adjust cell padding
                textColor: [0, 0, 0], // Set text color for the body
                rowPageBreak: 'avoid', // Avoid row page breaks
            },
            margin: { top: 10, left: 13 },
        });

        const summaryYStart = pdf.internal.pageSize.getHeight() - 50;

        pdf.setFont('Newsreader', 'bold')
        pdf.text('Remarks  : ', 13, summaryYStart)
        pdf.setFont('Newsreader', 'noraml')
        pdf.text('Thanking You,', 13, summaryYStart + 20)
        pdf.text('Yours Faithfully,', 13, summaryYStart + 24)
        pdf.text('For ', 13, summaryYStart + 28)
        pdf.setFont('Newsreader', 'bold')
        pdf.text('Eco Protection Engineers ', 19, summaryYStart + 28)

    };
    const pageFooter = (pdf: any) => {
        const totalPages = pdf.internal.getNumberOfPages();
        for (let i = 1; i <= totalPages; i++) {
            pdf.line(10, 283, 200, 283)
            pdf.setPage(i);
            pdf.setFont('Newsreader');
            pdf.text(
                `Page ${i} of ${totalPages}`,
                185,
                pdf.internal.pageSize.getHeight() - 5
            );
        }
    }

    const generateVendorPDF = (vendor: any) => {
        const pdf = new jsPDF();
        pdf.setProperties({
            title: `Request For Quotation - ${vendor?.vendorName}`,
        });
        // Generate the common content
        pdf.setFontSize(10);

        // Generate the vendor-specific content
        pdf.setFont('Newsreader', 'bold');
        pdf.text(`${vendor?.vendorName + " - " + vendor?.vendorAddress}`, 13, 44);
        pdf.setFont('Newsreader', 'normal')
        pdf.text(`P.O BOX : ${vendor?.vendorPinCode || "N/A"}`, 13, 48);
        pdf.setFont('Newsreader', 'bold')
        pdf.text('Contact Person', 13, 52)
        pdf.setFont('Newsreader', 'normal')
        pdf.text(`${vendor?.contactPerson || "N/A"}`, 13, 56);
        pdf.addImage(callImage, 'PNG', 13, 57, 3, 3);
        pdf.text(`  ${vendor?.contactPersonMobNo || "N/A"}`, 16, 60);
        generateCommonContent(pdf)
        pageFooter(pdf)

        // Save the PDF for the current vendor
        pdf.save(`RFQ_${vendor?.vendorName}.pdf`);

        // const pdfDataUri = pdf.output('datauristring');
        // const newTab = window.open();
        // newTab?.document.write(`<iframe width='100%' height='100%' src='${pdfDataUri}'></iframe>`);

    };
    vendorData.forEach((vendor: any) => {
        generateVendorPDF(vendor);
    });

}

export default RequestForQuotation