import React, { useState } from 'react'
import Styles from '../../styles/stockOutwardAdd.module.scss'
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import Checkbox from '../ui/Checkbox';
import { format } from 'date-fns';
import DeleteIcon from '../menu/icons/deleteIcon';



const StoreOutwardAdd = () => {


    const [initialValues, setInitialValues] = useState({
        stock_outward_date: format(new Date(), 'yyyy-MM-dd'),
    });
    const [checked, setChecked] = useState(false)
    const [disable, setDisable] = useState(true)
    let rowIndex = 0;

    const formik = useFormik({
        initialValues,
        // validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {

            console.log("values", values);
        }
    });

    const handleCheckBoxChange = (e: any) => {
        const CheckboxValue = e.target.checked;
        setChecked(CheckboxValue);
        setDisable(!disable)
    }

    return (
        <div>
            <div className={Styles.text}>
                <div className={Styles.textStyle}>
                    <h3>Stock OutWard Add</h3>
                </div>
                <div className={Styles.textStyle}>
                    <h6>Stock OutWard...</h6>
                </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.container}>
                <div className={Styles.box}>
                    <form onSubmit={formik.handleSubmit}>
                        <div className={Styles.fields_container}>
                            <div className={Styles.fields_container_1}>
                                <div>
                                    <Input
                                        label="OutWardID"
                                        placeholder="STO-YYYY-"
                                        name="quantity"
                                        // mandatory={true}
                                        width="350px"
                                        // value={formik.values.quantity}
                                        onChange={formik.handleChange}
                                    // error={
                                    //     formik.touched.quantity && formik.errors.quantity
                                    // }
                                    />
                                </div>
                                <div>
                                    <AutoCompleteSelect
                                        label="Site"
                                        name="user_id"
                                        onChange={formik.handleChange}
                                        // value={formik.values.user_id}
                                        placeholder="Select from options"
                                        mandatory
                                        width="350px"
                                    // onSelect={(value) => {
                                    //     formik.setFieldValue('user_id', value);
                                    // }}
                                    // optionList={userData}
                                    // error={
                                    //     formik.touched.user_id &&
                                    //     formik.errors.user_id
                                    // }
                                    />
                                </div>
                            </div>
                            <div className={Styles.fields_container_2}>
                                <div>
                                    <AutoCompleteSelect
                                        label="Project"
                                        name="user_id"
                                        onChange={formik.handleChange}
                                        // value={formik.values.user_id}
                                        placeholder="Select from options"
                                        mandatory
                                        width="350px"
                                    // onSelect={(value) => {
                                    //     formik.setFieldValue('user_id', value);
                                    // }}
                                    // optionList={userData}
                                    // error={
                                    //     formik.touched.user_id &&
                                    //     formik.errors.user_id
                                    // }
                                    />
                                </div>
                                <div >
                                    <DatePicker
                                        label="OutWard Date"
                                        name="stock_outward_date"
                                        onChange={formik.handleChange}
                                        mandatory
                                        disabled={disable}
                                        width='350px'
                                        value={formik.values.stock_outward_date}
                                    // error={formik.touched.access_start_date && formik.errors.access_start_date}
                                    />
                                </div>


                            </div>
                            <div className={Styles.fields_container_3}>
                                <div>
                                    <Checkbox
                                        name="is_editable"
                                        checked={checked}
                                        onChange={(e) => handleCheckBoxChange(e)}
                                        label="Edit OutWard Date"
                                    />
                                </div>

                            </div>
                            <div className={Styles.dividerStyle}></div>
                            <div className={Styles.tableContainer}>
                                <div className={Styles.secondHeader}>
                                    <div>
                                        <h3>Item Details</h3>
                                    </div>
                                    <div>
                                        <Button
                                            color="primary"
                                            shape="rectangle"
                                            justify="center"
                                            size="small"
                                            icon={<AddIcon />}
                                        >
                                            Add
                                        </Button>
                                    </div>
                                </div>

                                <table className={Styles.scrollable_table}>
                                    <thead>
                                        <tr>
                                            <th className={Styles.tableHeading}>S No</th>
                                            <th className={Styles.tableHeading}>ITEM</th>
                                            <th className={Styles.tableHeading}>QUANTITY</th>
                                            <th className={Styles.tableHeading}>IN STOCK</th>
                                            <th className={Styles.tableHeading}>UOM</th>
                                            <th className={Styles.tableHeading}>Action</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {/* {bomConfig?.map((item: any, index: any) => {
                                        rowIndex = rowIndex + 1;
                                        return (
                                            <>
                                                <tr>
                                                    <td>{rowIndex}</td>
                                                    <td>{item?.bom_name}</td>
                                                    <td>{item?.bom_description}</td>
                                                    <td>{item?.bom_type_name}</td>
                                                    <td>{item?.budget}</td>
                                                    <td>
                                                        <div
                                                            className={Styles.addPlan}
                                                            onClick={() => {
                                                                navigate(
                                                                    `/bomlist/${routeParams.id}/${item?.bom_configuration_id}`
                                                                );
                                                            }}
                                                            style={{
                                                                pointerEvents:
                                                                    `${item?.bom_configuration_id}` === ''
                                                                        ? 'none'
                                                                        : 'auto',
                                                            }}
                                                        >
                                                            <AddIcon style={{ height: '15px', width: '15px' }} />
                                                            <p className={Styles.addText}>Add BOM</p>
                                                        </div>
                                                    </td>
                                                </tr>
                                            </>
                                        );
                                    })} */}
                                        <tr>
                                            <td>{rowIndex + 1}</td>
                                            <td>
                                                <AutoCompleteSelect
                                                    defaultLabel=""
                                                    // width="250px"
                                                    name="bom_type_id"
                                                    mandatory={true}
                                                    // optionList={getBomType}
                                                    // value={formik.values.bom_type_id}
                                                    onChange={formik.handleChange}
                                                // onSelect={(value) => {
                                                //     formik.setFieldValue('bom_type_id', value);
                                                // }}
                                                // error={
                                                //     formik.touched.bom_type_id && formik.errors.bom_type_id
                                                // }
                                                />

                                            </td>
                                            <td>
                                                <Input
                                                    name="bom_description"
                                                    // value={formik.values.bom_description}
                                                    width='140px'
                                                    onChange={formik.handleChange}
                                                // error={
                                                //     formik.touched.bom_description &&
                                                //     formik.errors.bom_description
                                                // }
                                                />
                                            </td>
                                            <td>
                                               
                                                <Input
                                                    name="bom_name"
                                                    width='140px'
                                                    // value={formik.values.bom_name}
                                                    onChange={formik.handleChange}
                                                // error={formik.touched.bom_name && formik.errors.bom_name}
                                                />
                                               
                                            </td>
                                            <td>
                                                <Input
                                                    name="bom_name"
                                                    width='140px'
                                                    // value={formik.values.bom_name}
                                                    onChange={formik.handleChange}
                                                // error={formik.touched.bom_name && formik.errors.bom_name}
                                                />
                                            </td>
                                            <td>
                                                <div
                                                    style={{
                                                        cursor: 'pointer',
                                                        // paddingBottom: '20px',
                                                    }}
                                                >
                                                    <div >
                                                        <DeleteIcon />
                                                    </div>
                                                </div>
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                            </div>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    )
}

export default StoreOutwardAdd