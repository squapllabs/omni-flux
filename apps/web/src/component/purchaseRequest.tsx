import React, { useState } from 'react'
import Styles from '../styles/projectSettings.module.scss'
import Input from './ui/Input';
import Button from './ui/Button';
import { useFormik } from 'formik';
import AddIcon from './menu/icons/addIcon';
import SearchIcon from './menu/icons/search';
import AutoCompleteSelect from './ui/AutoCompleteSelect';
import { useParams, useNavigate } from 'react-router-dom';
import SelectNew from './ui/selectNew';
import CustomQuoteReqPopup from './ui/CustomPurchaseRequestPopup';


const PurchaseRequest = () => {

    const routeprops = useParams();
    const [quoteReqPopup, setQuoteReqPopup] = useState(false);
    const [initialValues, setInitialValues] = useState({
        project_role_id: '',
    });
    const [selectedValue, setSelectedValue] = useState('');
    const [dropSelect,setDropSelect] = useState();

    console.log("selectedValue", selectedValue);

    const handleDropChange = ( event: React.ChangeEvent<HTMLSelectElement>
        ) => {
            const selectedVId = event.target.value;
            setSelectedValue(selectedVId);
            console.log("selectedVId",selectedVId);
            

    }

    // const [selectedValue, setSelectedValue] = useState();
    const optionList: any =
        [{ value: "2", label: "Manual" }, { value: "1", label: "Based on Quotation" }, { value: "3", label: "example" }]



    const vendorSelect: any =
        [{ value: "Manual", label: "Manual" }, { value: "Based on Quotation", label: "Based on Quotation" }]

    const formik = useFormik({
        initialValues,
        // validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {

            console.log("values", values);

        },
    });

    const handleDropdownChange = (
        event: React.ChangeEvent<HTMLSelectElement>
    ) => {
        const selectedRoleId = event.target.value;
        setSelectedValue(selectedRoleId);
    };

    return (
        <div className={Styles.conatiner}>
            <div className={Styles.box}>
                <div className={Styles.textContent}>
                    <h3>Purchase Request</h3>
                </div>
                <form onSubmit={formik.handleSubmit}>
                    <div className={Styles.fields_container}>
                        <div className={Styles.fields_container_1}>
                            <div>
                                <SelectNew
                                    label="Vendor Selection Method"
                                    name="parent_master_data_id"
                                    defaultLabel="select the options"
                                    onChange={handleDropdownChange}
                                    value={selectedValue}
                                    // helperText="Depending on selected Lead Type below form will change"
                                    width='350px'
                                    disabled={routeprops.type != undefined ? true : false}
                                >
                                    {vendorSelect.map((option: any) => (
                                        <option key={option.value} value={option.value}>
                                            {option.label}
                                        </option>
                                    ))}
                                </SelectNew>
                            </div>
                            <div>
                             
                            </div>
                        </div>
                    </div>
            
                        <Button
                            color="secondary"
                            shape="rectangle"
                            justify="center"
                            size="small"
                            icon={<AddIcon />}
                            onClick={() => { setQuoteReqPopup(true) }}
                        >
                            Quotation
                        </Button>
                       
                    
                </form>
            </div>
            <CustomQuoteReqPopup
                isVissible={quoteReqPopup}
                onAction={setQuoteReqPopup}
            >
            </CustomQuoteReqPopup>
        </div>

    )
}

export default PurchaseRequest