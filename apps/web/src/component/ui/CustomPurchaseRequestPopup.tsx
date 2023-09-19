import React, { useState } from 'react'
import Button from '../ui/Button';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import TextArea from '../ui/CustomTextArea';
import Styles from '../../styles/purchaseRequestPopup.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from './AutoCompleteSelect';
import AutoCompleteMultiSelect from './AutoCompleteMultiSelect';
import AddIcon from '../menu/icons/addIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useGetAllVendors }  from '../../hooks/vendor-hooks';

const CustomPurchaseRequestPopup = (props: { isVissible: any, onAction: any }) => {
    const { isVissible, onAction } = props
    const [initialValues, setInitialValues] = useState({
        vendor_id: '',
        description: ''
    })
    const [selectedValue, setSelectedValue] = useState([]);
    const optionList: any =
        [{ value: "2", label: "Manual" }, { value: "1", label: "Based on Quotation" }, { value: "3", label: "example" }]
    const { data: getAllVendorsData = [], isLoading: dropLoading } = useGetAllVendors();

    const formik = useFormik({
        initialValues,
        // validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {
            console.log("values", values);
        },
    });

    console.log("formik",formik.values);
    // console.log("getAllVendorsData",getAllVendorsData);

    
    const handleCloseForm = () => {
        onAction(false);
        formik.resetForm();
    };

    const handleDropChange = (e) => {

    }


    return (
        <div>
            <div>
                {isVissible && (
                    <CustomPopup >
                        <div className={Styles.popupContent}>
                            <form onSubmit={formik.handleSubmit}>
                                <div className={Styles.header}>
                                    <div>
                                        <h4>Request for Quotation</h4>
                                        <span className={Styles.content}>
                                            Raise your purchase request againest your Project
                                        </span>
                                    </div>
                                    <div>
                                        <CloseIcon onClick={handleCloseForm} />
                                    </div>
                                </div>
                                <div className={Styles.dividerStyle}></div>
                                {/* <div className={Styles.inputFields}> */}
                                <div className={Styles.fields_container}>
                                    <div className={Styles.fields_container_1}>
                                        <div>
                                            <AutoCompleteMultiSelect
                                                label="Vendors"
                                                name="vendor_id"
                                                onChange={formik.handleChange}
                                                value={formik.values.vendor_id}
                                                placeholder="Select from options"
                                                mandatory
                                                width="350px"
                                                onSelect={(value) => {
                                                    // alert(value);
                                                    formik.setFieldValue('vendor_id', value);
                                                    console.log("value",value);
                                                    
                                                    
                                                }}
                                            optionList={getAllVendorsData}
                                            // error={
                                            //   formik.touched.user_id &&
                                            //   formik.errors.user_id
                                            // }
                                            />
                                        </div>


                                    </div>
                                    <div className={Styles.fields_container_2}>
                                        <div>
                                            <AutoCompleteSelect
                                                label="Items"
                                                name="user_id"
                                                onChange={formik.handleChange}
                                                // value={formik.values.user_id}
                                                placeholder="Select from options"
                                                mandatory
                                                width="350px"
                                                onSelect={(value) => {
                                                    formik.setFieldValue('user_id', value);
                                                }}
                                            // optionList={userData}
                                            // error={
                                            //   formik.touched.user_id &&
                                            //   formik.errors.user_id
                                            // }
                                            />
                                        </div>
                                        <div>
                                            <Input
                                                label="Quantity"
                                                name="user_id"
                                                onChange={formik.handleChange}
                                                // value={formik.values.user_id}
                                                // placeholder="Select from options"
                                                // mandatory
                                                width="350px"
                                                onSelect={(value) => {
                                                    formik.setFieldValue('user_id', value);
                                                }}
                                            // optionList={userData}
                                            // error={
                                            //   formik.touched.user_id &&
                                            //   formik.errors.user_id
                                            // }
                                            />
                                        </div>
                                        <div>
                                            <Button
                                                color="primary"
                                                shape="rectangle"
                                                justify="center"
                                                size="small"
                                                type="submit"
                                                icon={<AddIcon />}
                                            >
                                                Add
                                            </Button>

                                        </div>
                                    </div>
                                </div>

                                <div className={Styles.tableContainer}>
                                    <div>
                                        <table className={Styles.scrollable_table}>
                                            <thead>
                                                <tr>
                                                    <th className={Styles.tableHeading}>S NO</th>
                                                    <th className={Styles.tableHeading}>Vendor Name</th>
                                                    <th className={Styles.tableHeading}>Item</th>
                                                    <th className={Styles.tableHeading}>Quantity</th>
                                                    <th className={Styles.tableHeading}>Action</th>
                                                </tr>
                                            </thead>
                                            <tbody>
                                                <tr>
                                                    <td>1</td>
                                                    <td>sample</td>
                                                    <td>data</td>
                                                    <td>100</td>
                                                    <td>
                                                        <div className={Styles.tablerow}>
                                                            <DeleteIcon
                                                            // onClick={() =>
                                                            //     deleteProjectMember(data?.project_member_association_id)
                                                            // }
                                                            />
                                                        </div>
                                                    </td>
                                                </tr>
                                            </tbody>
                                        </table>
                                    </div>
                                </div>
                                <div className={Styles.dividerStyle}></div>
                                <div className={Styles.formButton}>
                                    <div>
                                        <Button
                                            className={Styles.cancelButton}
                                            shape="rectangle"
                                            justify="center"
                                            size="small"
                                            onClick={handleCloseForm}
                                        >
                                            Cancel
                                        </Button>
                                    </div>
                                    <div>
                                        <Button
                                            color="primary"
                                            shape="rectangle"
                                            justify="center"
                                            size="small"
                                            type="submit"
                                            icon={<AddIcon/>}
                                        >
                                            Raise Purchase Request
                                        </Button>
                                    </div>
                                </div>
                            </form>
                        </div>
                    </CustomPopup>)
                }
            </div>
        </div>
    )
}

export default CustomPurchaseRequestPopup