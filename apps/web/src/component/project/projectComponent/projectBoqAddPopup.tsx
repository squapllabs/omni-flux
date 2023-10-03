import React, { useState, useEffect } from 'react'
import Styles from '../../../styles/projectSettings.module.scss';
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import CustomSnackBar from '../../ui/customSnackBar';
import * as yup from 'yup';
import TextArea from '../../ui/CustomTextArea';
import {
    getByMasterDataProjectIdDrop,
} from '../../../hooks/masertData-hook';
import {
    getCreateBoQValidateyup,
} from '../../../helper/constants/boq-constants';
import { createBoQ,updateBoQ } from '../../../hooks/bom-hooks';
import BOMService from '../../../service/bom-service';


const ProjectBoqAddPopup = (props: any) => {

    const [initialValues, setInitialValues] = useState({
        bom_name: '',
        bom_description: '',
        bom_type_id: '',
        bom_type_name: '',
        project_id: props.projectId,
        budget: '',
        created_by: '',
        bom_configuration_id:''
    });
    const [openSnack, setOpenSnack] = useState(false);
    const [message, setMessage] = useState('');
    const { mutate: createNewProjectBoQ } = createBoQ();
    const { mutate: updateNewProjectBoQ } = updateBoQ();
    const { data: getBomType = [] } = getByMasterDataProjectIdDrop(props?.projectId);
    const validationSchema = getCreateBoQValidateyup(yup);

    const handleSnackBarClose = () => {
        setOpenSnack(false);
    };
    const handleClose = () => {
        props.setOpen(false);
    };

    useEffect(() => {
        if (props.mode === 'EDIT') {
            const fetchOne = async () => {
                const data = await BOMService.getBOQId(props.boqId);
                setInitialValues({
                    bom_configuration_id:data?.data?.bom_configuration_id,
                    bom_name: data?.data?.bom_name,
                    bom_description: data?.data?.bom_description,
                    bom_type_id: data?.data?.bom_type_id,
                    bom_type_name: data?.data?.bom_type_data?.master_data_name,
                    project_id: data?.data?.projectId,
                    budget: data?.data?.budget,
                    created_by: ''
                });
            };
            fetchOne();
        }
    }, []);

    const formik = useFormik({
        initialValues,
        validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {
            if (props?.boqId) {
                const Object: any = {
                    bom_configuration_id:values?.bom_configuration_id,
                    bom_name: values?.bom_name,
                    bom_description: values?.bom_description,
                    bom_type_id: values?.bom_type_id,
                    project_id: values?.project_id,
                };
                updateNewProjectBoQ(Object, {
                    onSuccess: (data, variables, context) => {
                        if (data?.message === 'success') {
                            setMessage('BoQ Updtated');
                            setOpenSnack(true);
                            setTimeout(() => {
                                handleClose();
                            }, 1000);
                            resetForm();
                        }
                    },
                });
            }
            else {
                const Object: any = {
                    bom_name: values.bom_name,
                    bom_description: values.bom_description,
                    bom_type_id: values.bom_type_id,
                    project_id: props.projectId,
                };
                createNewProjectBoQ(Object, {
                    onSuccess: (data, variables, context) => {
                        if (data?.message === 'success') {
                            setMessage('BoQ Created');
                            setOpenSnack(true);
                            setTimeout(() => {
                                handleClose();
                            }, 1000);
                            resetForm();
                        }
                    },
                });
            }
        },

    });

    return (
        <div>
            <form onSubmit={formik.handleSubmit}>
                <div className={Styles.divOne}>
                    <div>
                        <div>
                            <Input
                                name="bom_name"
                                label="BoQ Name"
                                width="350px"
                                mandatory={true}
                                value={formik.values.bom_name}
                                onChange={formik.handleChange}
                                error={formik.touched.bom_name && formik.errors.bom_name}
                            />
                        </div>
                        <div>
                            <TextArea
                                name="bom_description"
                                label='BoQ Description'
                                width="300px"
                                mandatory={true}
                                value={formik.values.bom_description}
                                onChange={formik.handleChange}
                                rows={5}
                                maxCharacterCount={1000}
                                error={
                                    formik.touched.bom_description &&
                                    formik.errors.bom_description
                                }
                            />
                        </div>
                        <div>
                            <AutoCompleteSelect
                                defaultLabel="Select from options"
                                width="350px"
                                label='BoQ Type'
                                name="bom_type_id"
                                mandatory={true}
                                optionList={getBomType}
                                value={formik.values.bom_type_id}
                                onChange={formik.handleChange}
                                onSelect={(value) => {
                                    formik.setFieldValue('bom_type_id', value);
                                    const matchingObjects = getBomType.filter(
                                        (obj: any) => Number(obj.value) === Number(value)
                                    );
                                    formik.setFieldValue(
                                        'bom_type_name',
                                        matchingObjects[0]?.label
                                    );
                                }}
                                error={
                                    formik.touched.bom_type_id && formik.errors.bom_type_id
                                }
                            />
                        </div>
                    </div>
                    <div >
                        <img src="/boq-add.png" className={Styles.imageBoQ}></img>
                    </div>

                </div>

                <div className={Styles.footer}>
                    <div className={Styles.dividerStyle}></div>
                    <div className={Styles.button}>
                        <Button
                            shape="rectangle"
                            justify="center"
                            size="small"
                            onClick={handleClose}
                            className={Styles.cancelButton}
                        >
                            Cancel
                        </Button>
                        <Button
                            shape="rectangle"
                            color="primary"
                            justify="center"
                            size="small"
                            type='submit'
                        >
                            Save
                        </Button>

                    </div>
                </div>
            </form>
            <CustomSnackBar
                open={openSnack}
                message={message}
                onClose={handleSnackBarClose}
                autoHideDuration={1000}
                type="success"
            />
        </div>
    )
}

export default ProjectBoqAddPopup

