import React from 'react'
import Styles from '../../styles/newStyles/globalExpense.module.scss';

const GlobalExpensesView = (props: any) => {

    const documentURL = props?.viewDocs?.bill_details[0]?.path;

    const getFileTypeFromUrl = () => {
        if (documentURL) {
            const lowercasedUrl = documentURL?.toLowerCase();
            if (lowercasedUrl?.includes('.png') || lowercasedUrl?.includes('.jpg') || lowercasedUrl?.includes('.jpeg') || lowercasedUrl?.includes('.gif') || lowercasedUrl?.includes('.bmp') || lowercasedUrl?.includes('.svg')) {
                return 'image';
            } else if (lowercasedUrl?.includes('.pdf')) {
                return 'pdf';
            } else if (lowercasedUrl.includes('.xls') || lowercasedUrl.includes('.xlsx') || lowercasedUrl.includes('.csv')) {
                return 'excel';
            }
            else {
                return 'unknown';
            }
        }
    }
    const fileType = getFileTypeFromUrl(); 

    return (

        <div>
            <h3>{`Site Expense Information for ${props?.viewDocs?.expense_master_data?.master_data_name ? props?.viewDocs?.expense_master_data?.master_data_name : props?.viewDocs?.site_expense_name} Category`}</h3>
            <div
                className={Styles.header}
            >
                <div className={Styles.content_container_left}>
                    <div className={Styles.content_container}>
                        <span> <b>Bill Number :</b> {props?.viewDocs?.bill_number ? props?.viewDocs?.bill_number : "Not Provided"}</span>
                        <span><b>Description :</b> {props?.viewDocs?.description ? props?.viewDocs?.description : "Not Provided"}</span>
                    </div>
                    <span><b>Amount : </b>₹  {props?.viewDocs?.total ? props?.viewDocs?.total : "Not Provided"}</span>
                </div>
            </div>
            {documentURL ?
                (fileType === "image" ?
                    < img src={documentURL} style={{ maxWidth: "700px", height: "auto" }} >
                    </img>
                    : <iframe src={documentURL} style={{ width: "800px", height: "400px" }} >
                    </iframe>)
                : "No Document Found"}
        </div >
    )
}

export default GlobalExpensesView;