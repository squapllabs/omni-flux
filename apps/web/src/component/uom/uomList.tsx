import React, { useState } from 'react';
import Styles from '../../styles/userList.module.scss';
import { IconButton } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import { useGetAlluom, useDeleteUom } from '../../hooks/uom-hooks';
import UomForm from './uomForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import CustomDialog from '../ui/customDialog';
import Button from '../ui/Button';
import { useFormik } from 'formik';
import { createuom } from '../../hooks/uom-hooks';
import Input from '../ui/Input';

const UomList = () => {
  const { data: getAlluom } = useGetAlluom();
  const { mutate: getDeleteuomByID } = useDeleteUom();
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [uomId, setUomID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');

  const handleClose = () => {
    setOpen(false);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const handleEdit = (event: React.FormEvent, value: any) => {
    setMode('EDIT');
    setUomID(value);
    setOpen(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteUom = (event: React.FormEvent, value: any) => {
    getDeleteuomByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const [initialValues, setInitialValues] = useState({
    uom_id: '',
    name: '',
    description: '',
  });
  const { mutate: createNewuom } = createuom();

  const formik = useFormik({
    initialValues,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          uom_id:values.uom_id,
          name: values.name,
          description: values.description,
        };
        createNewuom(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              setMessage('UOM created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });

  return (
    <div>
      <div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Add New UOM</h3>
            <span className={Styles.content}>
              Manage your UOM details here.
            </span>
          </div>
          <form onSubmit={formik.handleSubmit}>
            <div className={Styles.fields}>
              <div>
                <Input
                  label="Unit Of Measurement"
                  placeholder="Enter unit of measurement"
                  name="name"
                  value={formik.values.name}
                  onChange={formik.handleChange}
                  error={formik.touched.name && formik.errors.name}
                  width="100%"
                />
              </div>
              <div>
                <Input
                  name="description"
                  label="Description"
                  placeholder="Enter description"
                  value={formik.values.description}
                  onChange={formik.handleChange}
                  error={formik.touched.description && formik.errors.description}
                />
              </div>

              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                >
                  Add New UOM
                </Button>
              </div>
            </div>
          </form>
        </div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>List of Unit of Measurements</h3>
            <span className={Styles.content}>
              Manage your UOM details here.
            </span>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table>
                <thead>
                  <tr>
                    <th>UOM Name</th>
                    <th>Description</th>
                    <th>Options</th>
                  </tr>
                </thead>
                <tbody>
                  {getAlluom?.map((data: any) => (
                    <tr>
                      <td>{data.name}</td>
                      <td>{data.description}</td>
                      <td>
                        <IconButton
                          onClick={(e) => handleEdit(e, data.uom_id)}
                        >
                          <EditIcon />
                        </IconButton>
                        <IconButton
                          onClick={(e) =>
                            deleteUom(e, data.uom_id)
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
          <CustomDialogBox
            open={open}
            handleClose={handleClose}
            title="UOM Form"
            content={
              <UomForm
                setOpen={setOpen}
                open={open}
                setReload={setReload}
                mode={mode}
                uomId={uomId}
                setOpenSnack={setOpenSnack}
                setMessage={setMessage}
              />
            }
          />
          <CustomDialog
            open={openDelete}
            handleClose={handleCloseDelete}
            title="Delete UOM"
            content="Are you want to delete this UOM?"
            handleConfirm={deleteUom}
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
    </div>
  );
};

export default UomList;
