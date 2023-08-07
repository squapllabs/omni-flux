import React, { useState } from 'react';
import Styles from '../../styles/gstList.module.scss';
import { useGetAllGst, useDeleteGst } from '../../hooks/gst-hooks';
import CustomSnackBar from '../ui/customSnackBar';
import DeleteIcon from '../menu/icons/deleteIcon';
import EditIcon from '../menu/icons/editIcon';
import CustomEditDialog from '../ui/customEditDialogBox';
import GstForm from './gstCreate';
import Button from '../ui/Button';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import { createGst } from '../../hooks/gst-hooks';
import * as Yup from 'yup';
import { getGstcreationYupschema } from '../../helper/constants/gst-constants';
import CustomLoader from '../ui/customLoader';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox'

//Function for GST
const GstList = () => {
  const { data: getAllGstData, isLoading: getAllLoading } = useGetAllGst();
  const { mutate: getDeleteGstByID } = useDeleteGst();
  const { mutate: createNewGst } = createGst();
  const [initialValues, setInitialValues] = useState({
    gst_id: '',
    rate: '',
  });
  const [open, setOpen] = useState(false);
  const [mode, setMode] = useState('');
  const [reload, setReload] = useState(false);
  const [gstId, setGstId] = useState();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const validationSchema = getGstcreationYupschema(Yup);
  const [value,setValue] = useState(); 

  //Function for closing delete popup
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  //Function for opening snackbar
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  //Function for calling popup of delete
  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  //Function for deleting a gst data
  const deleteGst = () => {
    getDeleteGstByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };
  //Function for editing gst
  const handleEdit = (value: any) => {
    setMode('EDIT');
    setGstId(value);
    setOpen(true);
  };
  //Function for adding new gst data
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          gst_id: values.gst_id,
          rate: parseFloat(values.rate)
        };
        createNewGst(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              setMessage('GST created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    }
  });

  return (
    <div>
      <div>
        <CustomLoader
          loading={getAllLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>Add New GST</h3>
              <span className={Styles.content}>
                Manage your GST details here.
              </span>
            </div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.fields}>
                <div>
                  <Input
                    label="Gst Rate"
                    placeholder="Enter gst rate"
                    name="rate"
                    value={formik.values.rate}
                    onChange={formik.handleChange}
                    error={formik.touched.rate && formik.errors.rate}
                    width="100%"
                  />
                </div>

                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon />}
                  >
                    Add New GST
                  </Button>
                </div>
              </div>
            </form>
          </div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of GST</h3>
              <span className={Styles.content}>
                Manage your GST details here.
              </span>
            </div>
            <div className={Styles.tableContainer}>
              <div>
                <table>
                  <thead>
                    <tr>
                      <th>S No</th>
                      <th>GST Rate</th>
                      <th>Option</th>
                    </tr>
                  </thead>
                  <tbody>
                    {getAllGstData?.map((data: any,index:number) => (
                      <tr>
                        <td>{index+1}</td>
                        <td>{data.rate}</td>
                        <td>
                          <EditIcon onClick={() => handleEdit(data.gst_id)} />
                          
                          {/* <DeleteIcon onClick={() =>
                            deleteCategoryHandler( data.gst_id)}/> */}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </CustomLoader>
        <CustomDelete
          open={openDelete}
          title="Delete GST"
          contentLine1="Are you sure you want to delete this post? This action cannot be undone."
          contentLine2="Deleted GST will move to Inactive tab."
          handleClose={handleCloseDelete}
          handleConfirm={deleteGst}
        />
        <CustomEditDialog
          open={open}
          // handleClose={handleClose}
          // title="Edit Gst"
          content={
            <GstForm
              setOpen={setOpen}
              open={open}
              setReload={setReload}
              mode={mode}
              gstId={gstId}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </div>
    </div>
  );
};

export default GstList;
