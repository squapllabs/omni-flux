import React from 'react'
import Styles from '../../../../styles/newStyles/projectSiteExpense.module.scss';

const SiteExpensesView = (props: any) => {
    // console.log("test", props);

    const getFileTypeFromUrl = () => {
        // Convert the URL to lowercase for case-insensitive matching
        const lowercasedUrl = props?.viewDocs?.bill_details[0]?.path?.toLowerCase();
        if (lowercasedUrl?.includes('.png') || lowercasedUrl?.includes('.jpg') || lowercasedUrl?.includes('.jpeg') || lowercasedUrl?.includes('.gif') || lowercasedUrl?.includes('.bmp') || lowercasedUrl?.includes('.svg')) {
            return 'image';
        } else if (lowercasedUrl?.includes('.pdf')) {
            return 'pdf';
        } else {
            return 'unknown';
        }
    }
    // const url = props?.viewDocs?.bill_details[0]?.path;
    const fileType = getFileTypeFromUrl();
    // console.log(`File type: ${fileType}`);


    return (

        <div>
            <h3>Site Expense Information for {props?.viewDocs?.site_expense_name} Category</h3>
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

            {fileType === "image" ?
                < img src={props?.viewDocs?.bill_details[0]?.path} style={{ maxWidth: "700px", height: "auto" }} >
                </img>
                : <iframe src={props?.viewDocs?.bill_details[0]?.path} style={{ width: "800px", height: "400px" }} >
                </iframe>}
        </div >

    )
}

export default SiteExpensesView