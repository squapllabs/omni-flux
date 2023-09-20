import React, { useEffect, useState } from 'react'
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
import { useGetAllVendors } from '../../hooks/vendor-hooks';
import { createPurchaseRequest } from '../../hooks/purchaseRequest-hooks';
import PurchaseRequestService from '../../service/purchaseRequest-service';

const CustomPurchaseRequestPopup = (props: { isVissible: any, onAction: any }) => {
    const { isVissible, onAction } = props
    const [itemValues, setItemsValues] = useState([]);
    let rowIndex = 0;
    const [itemsData, setItemsData] = useState()
    const [purchaseRequestData, setPurchaseRequestData] = useState<any>([]);
    const [initialValues, setInitialValues] = useState({
        vendor_id: '',
        item_id: '',
        quantity: '',
        item_name: ''
    })
    console.log("pr", purchaseRequestData);

    const { data: getAllVendorsData = [], isLoading: dropLoading } = useGetAllVendors();
    const { mutate: createNewPurchaseRequest } = createPurchaseRequest();

    const formik = useFormik({
        initialValues,
        // validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {
            console.log("values", values);
            let arr = [];
            arr = [...purchaseRequestData, values]
            setPurchaseRequestData(arr);

        },
    });

    const handleCloseForm = () => {
        onAction(false);
        formik.resetForm();
    };

    const handleDropChange = async (obj: any) => {
        const itemsData = await PurchaseRequestService.getProjectItems(137)
        let arr: any = [];
        setItemsData(itemsData.data)
        let items = itemsData?.data?.map((items: any, index: any) => {
            let obj: any = {
                value: items?.item_id,
                label: items?.item_data?.item_name
            }
            arr.push(obj)
        })
        setItemsValues(arr);
        if (!formik.values.item_id) {
            formik.values.quantity = ''
        }
    }

    const deletePurchaseRequest = (index: number) => {
        console.log("i", index);

        purchaseRequestData.splice(index, 1)
        setPurchaseRequestData([...purchaseRequestData])
    }

    const handleSubmit = () => {

        let data = purchaseRequestData?.map((data: any) => {
            let vendorId = data?.vendor_id?.map((vendor: any) => vendor.value)
            console.log("v", vendorId);
            // return vendorId
      
        console.log("vvvv",data.vendorId);
        
            let object = {
                indent_request_id: 10,
                "requester_user_id": 1,
                // "request_date": "2023-09-19",
                status: "Waiting For Quotation",
                "project_id": 137,
                vendor_ids: vendorId,
                // "total_cost": 1000000,
                // "created_by": 1,
                purchase_request_details: [
                    {
                        "item_id": data?.item_id,
                        "quantity": Number(data?.quantity),
                        "item_name": data?.item_name
                    }
                ]
            }
            // return object;
            console.log("obj",object);
       
      
        
        createNewPurchaseRequest(object, {
            onSuccess: (data, variables, context) => {
                if (data?.message === 'success') {
                    // setMessage('Labour created');
                    // setOpenSnack(true);
                    setTimeout(() => {
                        //   navigate('/labour');
                    }, 1000);
                }
            },
        });
    })
        console.log("purchaseRequestData", purchaseRequestData)
    }


    useEffect(() => {
        handleDropChange()
    }, [initialValues])

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
                                                    formik.setFieldValue('vendor_id', value);
                                                    console.log("value", value);
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
                                                name="item_id"
                                                onChange={formik.handleChange}
                                                value={formik.values.item_id}
                                                placeholder="Select from options"
                                                mandatory
                                                width="350px"
                                                onSelect={(value) => {
                                                    formik.setFieldValue('item_id', value);

                                                    const matchingObjects = itemsData?.filter(
                                                        (obj: any) => Number(obj.item_id) === Number(value)
                                                    );
                                                    formik.setFieldValue(
                                                        'quantity',
                                                        matchingObjects[0]?.quantity
                                                    );
                                                    formik.setFieldValue(
                                                        'item_name',
                                                        matchingObjects[0]?.item_data?.item_name
                                                    );
                                                    console.log("matchingObjects[0]?.item_name", matchingObjects);


                                                }}
                                                optionList={itemValues}
                                            // error={
                                            //   formik.touched.user_id &&
                                            //   formik.errors.user_id
                                            // }
                                            />
                                        </div>
                                        <div>
                                            <Input
                                                label="Quantity"
                                                placeholder="Enter Quantity"
                                                name="quantity"
                                                mandatory={true}
                                                width="350px"
                                                value={formik.values.quantity}
                                                onChange={formik.handleChange}
                                            // error={
                                            //     formik.touched.quantity && formik.errors.quantity
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
                                                {purchaseRequestData?.length === 0 ?

                                                    (<tr>
                                                        <td></td>
                                                        <td></td>
                                                        <td>No data found</td>
                                                        <td></td>
                                                        <td></td>
                                                    </tr>) :

                                                    purchaseRequestData?.map((item: any, index: any) => {
                                                        let vendorName = item?.vendor_id?.map((vendor: any) => vendor.label).join(', ')
                                                        rowIndex = rowIndex + 1;
                                                        return (
                                                            <>
                                                                <tr>
                                                                    <td>{rowIndex}</td>
                                                                    <td>{vendorName}</td>
                                                                    <td>{item.item_name}</td>
                                                                    <td>{item.quantity}</td>
                                                                    <td>
                                                                        <div className={Styles.tablerow}>
                                                                            <DeleteIcon
                                                                                onClick={() =>
                                                                                    deletePurchaseRequest(index)
                                                                                }
                                                                            />
                                                                        </div>
                                                                    </td>
                                                                </tr>
                                                            </>
                                                        )

                                                    })}

                                            </tbody>
                                        </table>
                                    </div>
                                </div>
                            </form>
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
                                        // type="submit"
                                        onClick={handleSubmit}
                                        icon={<AddIcon />}
                                    >
                                        Raise Purchase Request
                                    </Button>
                                </div>
                            </div>

                        </div>
                    </CustomPopup>)
                }
            </div>
        </div>
    )
}
export default CustomPurchaseRequestPopup