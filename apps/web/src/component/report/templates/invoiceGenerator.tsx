import jsPDF from 'jspdf';
import React from 'react';
import autoTable from 'jspdf-autotable';
const MyComponent: React.FC = () => {
  const generatePDF = () => {
    const invoices = {
      name: 'Dinesh B Choudhari	',
      bill_no: 3,
      date: '08-08-2023',
      address: 'Ward No - 7, Bakoiya, Majhiaon, Garhwa, Jharkhand - 822114',
      deleiveryNote: '',
      payment_mode: '',
      mob_no: '9750726210',
      gst: '',
      stateName: 'Bihar,Code:10',
      billPeriodFrom: '16.03.2023 to 31.03.2023',
      siteName: 'Patna DPS 2 - Patna',
      total: '250',
      grandTotal: '250',
      amount_inwords: '',
      pan_no: 'BEJPC7265R',
      Bank_details: '',
      account_name: 'Dinesh Choudhari',
      acount_no: '35217727362',
      bank_name: 'STATE BANK OF INDIA',
      ifsc_code: 'SBIN0006037',
    };
    const pdf = new jsPDF();
    const pdfWidth = pdf.internal.pageSize.getWidth();
    const pdfHeight = pdf.internal.pageSize.getHeight();

    // Simulate content for the header, footer, and main section
    const dummyHeaderContent = 'Header content'; // Your header content here
    const dummyFooterContent = 'Footer content'; // Your footer content here
    const dummyMainContent =
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ...'; // Your main content here

    // Calculate text height for given text and width
    const getTextHeight = (text, maxWidth) => {
      const lineHeight = 10; // Adjust this based on your font size
      const textLines = pdf.splitTextToSize(text, maxWidth);
      return textLines.length * lineHeight;
    };

    // Calculate header height
    const headerHeight = getTextHeight(dummyHeaderContent, pdfWidth);
    pdf.rect(0, 0, pdfWidth, headerHeight, 'S');
    pdf.text(15, headerHeight - 10, dummyHeaderContent);

    // Calculate footer height
    const footerHeight = getTextHeight(dummyFooterContent, pdfWidth);
    pdf.rect(0, pdfHeight - footerHeight, pdfWidth, footerHeight, 'S');
    pdf.text(15, pdfHeight - footerHeight + 10, dummyFooterContent);

    // Main Content
    const mainContentStartY = headerHeight;
    const mainContentEndY = pdfHeight - footerHeight;
    const maxMainContentHeight = mainContentEndY - mainContentStartY;

    const mainContentLines = pdf.splitTextToSize(dummyMainContent, pdfWidth);
    let currentY = mainContentStartY;
    let contentPage = 1;

    mainContentLines.forEach((line) => {
      const remainingHeight = mainContentEndY - currentY;

      if (remainingHeight < getTextHeight(line, pdfWidth)) {
        pdf.addPage();
        currentY = mainContentStartY;
        contentPage++;
      }

      pdf.text(15, currentY + 10, line);
      currentY += getTextHeight(line, pdfWidth);
    });

    if (contentPage > 1) {
      // Add page number to subsequent pages
      pdf.text(pdfWidth - 15, pdfHeight - 10, `Page ${contentPage}`);
    }

    pdf.save('example.pdf');
  };

  return (
    <div>
      {/* Your component content */}
      <h1>Hello, React with jsPDF!</h1>
      <p>Click the button below to generate a PDF.</p>
      <button onClick={generatePDF}>Generate PDF</button>
      <div style={{ marginTop: '10px' }}>
        <iframe
          title="PDF Preview"
          src="my-file.pdf"
          width="100%"
          height="500px"
        />
      </div>
    </div>
  );
};

export default MyComponent;
