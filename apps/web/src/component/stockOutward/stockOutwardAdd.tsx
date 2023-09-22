import React, { useEffect, useState } from 'react'
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
import ProjectService from '../../service/project-service';
import { getUserDataProjectRolebased, getByProjectId } from '../../hooks/project-hooks'
import { useNavigate } from 'react-router-dom';
import {
    getStockOutwardCreationYupschema,
    getStockOutwardItemCreationYupschema
} from '../../helper/constants/stockOutward-constants';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';



const StoreOutwardAdd = () => {
    const state: RootState = store.getState();
    const encryptedData = getToken(state, 'Data');
    const userData: any = encryptedData.userData;
    const siteEngineerName: any = userData.first_name + " " + userData.last_name
    const siteEngineerId: any = userData.user_id;
    console.log("siteEngineerName", userData);

    const [initialValues, setInitialValues] = useState({
        site_id: '',
        // site_engineer_id: Number(siteEngineerId),
        site_engineer_id:'',
        stock_outward_date: format(new Date(), 'yyyy-MM-dd'),
    });
    const [checked, setChecked] = useState(false)
    const [siteChecked, setSiteChecked] = useState(false)
    const [stockData, setStockData] = useState<any>([]);
    const [disable, setDisable] = useState(true)
    const [siteData, setSiteData] = useState();
    const [buttonDisable, setButtonDisable] = useState(false);




    const navigate = useNavigate();
    let rowIndex = 0;
    let Obj: any = {
        projectID: 137,
        role: 'Site Engineer',
    };
    const { data: getSiteEngineerData = [] } =
        getUserDataProjectRolebased(Obj);
    const { data: getProjectData } = getByProjectId(137);


    const validationSchema = getStockOutwardCreationYupschema(Yup);

    const formik = useFormik({
        initialValues,
        validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {
            console.log("valu", values);
            let arr = [];
            arr = [...stockData, values];
            setStockData(arr);
        }
    });


    const handleCheckBoxChange = (e: any) => {
        const CheckboxValue = e.target.checked;
        setChecked(CheckboxValue);
        setDisable(!disable)
    }

    const handleCheckBoxSiteChange = (e: any) => {
        const CheckboxValue = e.target.checked;
        setSiteChecked(CheckboxValue);
    }

    const fetchProjectSite = async () => {
        const siteData = await ProjectService.getOneProjectSite(137)
        let arr: any = [];
        let siteValues = siteData?.data?.map((site: any) => {
            let obj: any = {
                value: site?.site_id,
                label: site?.site_details?.name
            }
            arr.push(obj)
        })
        setSiteData(arr)
    }


    useEffect(() => {
        fetchProjectSite()
    }, [])

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
                                        disabled={true}
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
                                        name="site_id"
                                        onChange={formik.handleChange}
                                        value={formik.values.site_id}
                                        placeholder="Select from options"
                                        mandatory
                                        width="350px"
                                        onSelect={(value) => {
                                            formik.setFieldValue('site_id', value);
                                        }}
                                        optionList={siteData}
                                        error={
                                            formik.touched.site_id &&
                                            formik.errors.site_id
                                        }
                                    />
                                </div>
                                <div>
                                    <AutoCompleteSelect
                                        label="Site Engineer Name"
                                        name="site_engineer_id"
                                        onChange={formik.handleChange}
                                        value={formik.values.site_engineer_id }
                                        placeholder="Select from options"
                                        mandatory
                                        // disabled={siteChecked === false ? true : false}
                                        width="350px"
                                        onSelect={(value) => {
                                            formik.setFieldValue('site_engineer_id', value);
                                            // console.log("v", value);
                                        }}

                                        optionList={getSiteEngineerData}
                                        error={
                                            formik.touched.site_engineer_id &&
                                            formik.errors.site_engineer_id
                                        }
                                    />
                                    {/* <div>
                                        <Checkbox
                                            name="is_site"
                                            checked={siteChecked}
                                            onChange={(e) => handleCheckBoxSiteChange(e)}
                                        />
                                        <span className={Styles.checkBox}>  Edit Site Engineer Name</span>
                                    </div> */}
                                </div>

                            </div>
                            <div className={Styles.fields_container_2}>
                                <div>
                                    <Input
                                        label="Project"
                                        // name="user_id"
                                        width="350px"
                                        // onSelect={(value) => {
                                        //     formik.setFieldValue('user_id', value);
                                        // }}
                                        value={getProjectData?.project_name}
                                        disabled={true}
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
                                        // onChange={formik.handleChange}
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
                                    // label="Edit OutWard Date"
                                    />
                                    <span className={Styles.checkBox}>  Edit OutWard Date</span>
                                </div>

                            </div>
                            <div className={Styles.dividerStyle}></div>
                            <div className={Styles.tableContainer}>

                                <ItemDetailsTable stockData={stockData} setStockData={setStockData} />
                            </div>
                        </div>
                        <div className={Styles.buttonFields}>
                            <div>
                                <Button
                                    color="secondary"
                                    shape="rectangle"
                                    justify="center"
                                    size="small"
                                    disabled={stockData.length === 0 ? true : false}
                                    onClick={() => {
                                        // navigate('/labour');
                                    }}
                                >
                                    Back
                                </Button>
                            </div>
                            <div>
                                <Button
                                    color="primary"
                                    shape="rectangle"
                                    justify="center"
                                    size="small"
                                    disabled={stockData.length === 0 ? true : false}
                                >
                                    Save
                                </Button>
                            </div>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    )
}

export default StoreOutwardAdd


const ItemDetailsTable: React.FC = (props: {
    stockData: any;
    setStockData: any;
}) => {
    const { stockData, setStockData } = props;

    // console.log("s", stockData);

    let rowIndex = 0;
    const [initialValues, setInitialValues] = useState({
        item_id: '',
        outward_quantity: '',
        available_quantity: '100',
        name: 'Meter'
    });

    const validationSchema = getStockOutwardItemCreationYupschema(Yup);
    const itemData: any = [{ value: "169", label: "Item 121" }, { value: "159", label: "Item 6" }]

    // const handleChange = (
    //     event: React.ChangeEvent<HTMLInputElement>,
    //     index: number
    // ) => {
    //     let tempObj: any = {};
    //     tempObj = {
    //         ...stockData[index],
    //         [event.target.name]: event.target.value,
    //     };
    //     const tempArry = [...stockData];
    //     tempArry[index] = tempObj;
    //     setStockData(tempArry);
    // };

    const formik = useFormik({
        initialValues,
        validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {
            console.log("values", values);
            setStockData([...stockData, values])
            resetForm()
        }
    });



    return (
        <div >
            <form onSubmit={formik.handleSubmit}>
                <div className={Styles.secondHeader}>
                    <div>
                        <h3>Item Details</h3>
                    </div>
                    <div>
                        <Button
                            type='button'
                            color="primary"
                            shape="rectangle"
                            justify="center"
                            size="small"
                            onClick={() => formik.handleSubmit()}
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
                        {stockData?.map((items: any, index: any) => {
                            rowIndex = rowIndex + 1;
                            return (
                                <tr>
                                    <td>{rowIndex}</td>
                                    <td>{items.item_id}</td>
                                    <td>{items.outward_quantity}</td>
                                    <td>{items.available_quantity}</td>
                                    <td>{items.name}</td>
                                </tr>
                            );
                        })}
                        <tr>
                            <td>{rowIndex + 1}</td>
                            <td>
                                <AutoCompleteSelect
                                    placeholder="Select from options"
                                    // width="250px"
                                    name="item_id"
                                    mandatory={true}
                                    optionList={itemData}
                                    value={formik.values.item_id}
                                    onChange={formik.handleChange}
                                    onSelect={(value) => {
                                        formik.setFieldValue('item_id', value);
                                    }}

                                    error={
                                        formik.touched.item_id && formik.errors.item_id
                                    }
                                />

                            </td>
                            <td>
                                <Input
                                    name="outward_quantity"
                                    value={formik.values.outward_quantity}
                                    width='140px'
                                    onChange={formik.handleChange}
                                    error={
                                        formik.touched.outward_quantity &&
                                        formik.errors.outward_quantity
                                    }
                                />
                            </td>
                            <td>

                                <Input
                                    name="available_quantity"
                                    width='180px'
                                    value={formik.values.available_quantity}
                                    onChange={formik.handleChange}
                                    disabled={true}
                                    error={formik.touched.available_quantity && formik.errors.available_quantity}
                                />

                            </td>
                            <td>
                                <Input
                                    name="name"
                                    width='180px'
                                    disabled={true}
                                    value={formik.values.name}
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

            </form>
        </div>
    )

}