import React, { useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/customaddbom.module.scss';
import { createBom } from '../../hooks/bom-hooks';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import { getBomValidateyup } from '../../helper/constants/bom-constants';
import CustomSnackBar from '../ui/customSnackBar';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import { useGetAllItemsDrops } from '../../hooks/item-hooks';

const CustomClientAdd = (props: { isVissible: any; onAction: any }) => {
  const { isVissible, onAction } = props;
  const validationSchemaClient = getBomValidateyup(Yup);
  const { mutate: createNewBom } = createBom();
  const [clientinitialValues, setclientInitialValues] = useState({
    bom_name: '',
    uom_id: '',
    item_id: '',
    quantity: '',
  });
  const { data: getAllUomDataForDrop = [], isLoading: dropLoading } =
    useGetAllUomDrop();
  const { data: getAllItemDataForDrop = [], isLoading: dropLoadingItem } =
    useGetAllItemsDrops();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const formik = useFormik({
    initialValues: clientinitialValues,
    validationSchema: validationSchemaClient,
    enableReinitialize: true,
    onSubmit: (values) => {
      const Object: any = {
        quantity: Number(values.quantity),
        bom_name: values.bom_name,
        uom_id: Number(values.uom_id),
        item_id: Number(values.item_id),
      };
      console.log(Object)
    //   createNewBom(Object, {
    //     onSuccess: (data, variables, context) => {
    //       if (data?.success) {
    //         setMessage('Bom created');
    //         setOpenSnack(true);
    //         handleCloseForm();
    //       }
    //     },
    //   });
    },
  });

  const handleCloseForm = () => {
    onAction(false);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div>
      <div>
        {isVissible && (
          <CustomPopup
          className='sample'
          >
            <div className={Styles.popupContent}>
              <form onSubmit={formik.handleSubmit}>
                <div className={Styles.header}>
                  <div>
                    <h4>Add Bom</h4>
                  </div>
                  <div>
                    <CloseIcon onClick={handleCloseForm} />
                  </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                <div
                  style={{
                    display: 'flex',
                    flexDirection: 'row',
                    justifyContent: 'space-around',
                    paddingTop:'10px'
                  }}
                >
                  <div>
                    <Input
                      label="Name"
                      placeholder="Enter name"
                      name="bom_name"
                      mandatory={true}
                      value={formik.values.bom_name}
                      onChange={formik.handleChange}
                      error={formik.touched.bom_name && formik.errors.bom_name}
                      width="100%"
                    />
                  </div>
                  <div>
                    <Input
                      label="Quantity"
                      placeholder="Enter quantity"
                      name="quantity"
                      value={formik.values.quantity}
                      onChange={formik.handleChange}
                      error={formik.touched.quantity && formik.errors.quantity}
                      mandatory={true}
                      width="100%"
                    />
                  </div>
                </div>
                <div
                  style={{
                    display: 'flex',
                    flexDirection: 'row',
                    justifyContent: 'space-around',
                  }}
                >
                  <div>
                    <AutoCompleteSelect
                      label="Item Name"
                      name="item_id"
                      onChange={formik.handleChange}
                      value={formik.values.item_id}
                      placeholder="Select from options"
                      mandatory={true}
                      width="190px"
                      onSelect={(value) => {
                        formik.setFieldValue('item_id', value);
                      }}
                      optionList={
                        dropLoadingItem === true ? [] : getAllItemDataForDrop
                      }
                      error={formik.touched.item_id && formik.errors.item_id}
                    />
                  </div>
                  <div>
                    <AutoCompleteSelect
                      label="UOM"
                      name="uom_id"
                      onChange={formik.handleChange}
                      value={formik.values.uom_id}
                      placeholder="Select from options"
                      mandatory={true}
                      width="190px"
                      onSelect={(value) => {
                        formik.setFieldValue('uom_id', value);
                      }}
                      optionList={
                        dropLoading === true ? [] : getAllUomDataForDrop
                      }
                      error={formik.touched.uom_id && formik.errors.uom_id}
                    />
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
                    >
                      Submit
                    </Button>
                  </div>
                </div>
              </form>
            </div>
          </CustomPopup>
        )}
      </div>
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default CustomClientAdd;
