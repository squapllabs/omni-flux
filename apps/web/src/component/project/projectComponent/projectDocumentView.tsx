import React from 'react';
import Styles from '../../../styles/newStyles/projectSiteExpense.module.scss';

const ProjectDocumentView = (props: any) => {
  const documentURL = props?.viewDocs;
  const getFileTypeFromUrl = () => {
    if (documentURL) {
      const lowercasedUrl = documentURL?.toLowerCase();
      if (
        lowercasedUrl?.includes('.png') ||
        lowercasedUrl?.includes('.jpg') ||
        lowercasedUrl?.includes('.jpeg') ||
        lowercasedUrl?.includes('.gif') ||
        lowercasedUrl?.includes('.bmp') ||
        lowercasedUrl?.includes('.svg')
      ) {
        return 'image';
      } else if (lowercasedUrl?.includes('.pdf')) {
        return 'pdf';
      } else if (
        lowercasedUrl.includes('.xls') ||
        lowercasedUrl.includes('.xlsx') ||
        lowercasedUrl.includes('.csv')
      ) {
        return 'excel';
      } else {
        return 'unknown';
      }
    }
  };
  const fileType = getFileTypeFromUrl();

  return (
    <div>
      {documentURL ? (
        fileType === 'image' ? (
          <img
            src={documentURL}
            alt="document"
            style={{ width: '100%', height: '500px' }}
          ></img>
        ) : (
          <iframe
            src={documentURL}
            style={{ width: '1210px', height: '500px' }}
            title="Document Viewer"
          ></iframe>
        )
      ) : (
        'No Document Found'
      )}
    </div>
  );
};

export default ProjectDocumentView;
