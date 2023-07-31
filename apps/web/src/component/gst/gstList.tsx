import React, { useState } from 'react';
import Styles from '../../styles/gstList.module.scss';
import { useGetAllGst, useDeleteGst } from '../../hooks/gst-hooks';
import { IconButton } from '@mui/material';
import CustomDialog from '../ui/customDialog';
import MySnackbar from '../ui/MySnackbar';
import DeleteIcon from '../menu/icons/deleteIcon';
import EditIcon from '@mui/icons-material/Edit';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import GstForm from './gstCreate';
import Button from '../ui/Button';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import { createGst } from '../../hooks/gst-hooks';
import * as Yup from 'yup';
import { getGstcreationYupschema } from '../../helper/constants/gst-constants';

const GstList = () => {

  const [initialValues, setInitialValues] = useState({
    gst_id: '',
    rate: '',
  });
  const { data: getAllGstData, isLoading: loader } = useGetAllGst();

  const { mutate: getDeleteGstByID } = useDeleteGst();
  const [open, setOpen] = useState(false);
  const [mode, setMode] = useState('');
  const [reload, setReload] = useState(false);
  const [gstId, setGstId] = useState();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const validationSchema = getGstcreationYupschema(Yup);

  const handleClose = () => {
    setOpen(false);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const deleteGst = (event: React.FormEvent, value: any) => {
    getDeleteGstByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleEdit = (event: React.FormEvent, value: any) => {
    setMode('EDIT');
    setGstId(value);
    setOpen(true);
  };

  const { mutate: createNewGst } = createGst();

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
                >
                  Add New GST
                </Button>
              </div>
            </div>
          </form>
        </div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>List of Categories</h3>
            <span className={Styles.content}>
              Manage your GST details here.
            </span>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table>
                <thead>
                  <tr>
                    <th>GST Rate</th>
                    <th>Option</th>
                  </tr>
                </thead>
                <tbody>
                  {getAllGstData?.map((data: any) => (
                    <tr>
                      <td>{data.rate}</td>
                      <td>
                        <IconButton
                          onClick={(e) => handleEdit(e, data.gst_id)}
                        >
                          <EditIcon />
                        </IconButton>
                        <IconButton
                          onClick={(e) =>
                            deleteGst(e, data.gst_id)
                          }
                        >
                          <DeleteIcon />
                        </IconButton>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
        <CustomDialog
          open={openDelete}
          handleClose={handleCloseDelete}
          title="Delete Gst"
          content="Are you want to delete this gst?"
          handleConfirm={deleteGst}
        />
        <CustomDialogBox
          open={open}
          handleClose={handleClose}
          title="Gst Creation"
          content={
            <GstForm
              setOpen={setOpen}
              open={open}
              setReload={setReload}
              mode={mode}
              gstId={gstId}
              setOpenDeleteSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
        <MySnackbar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          severity={'success'}
          autoHideDuration={1000}
        />
      </div>
    </div>
  );
};

export default GstList;
