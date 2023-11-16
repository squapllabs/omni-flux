import React, { useState } from 'react';
import Styles from '../../styles/gstList.module.scss';
import { useGetAllGst, useDeleteGst } from '../../hooks/gst-hooks';
import CustomSnackBar from '../ui/customSnackBar';
import { createGst } from '../../hooks/gst-hooks';
import CustomLoader from '../ui/customLoader';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomPopup from '../ui/CustomSidePopup';
import GSTAddForm from './gstCreate'

/* Function for GST */
const GstList = () => {
  const { data: getAllGstData, isLoading: getAllLoading } = useGetAllGst();
  const { mutate: getDeleteGstByID } = useDeleteGst();
  const { mutate: createNewGst } = createGst();
  const [open, setOpen] = useState(false);
  const [mode, setMode] = useState('');
  const [reload, setReload] = useState(false);
  const [gstId, setGstId] = useState();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [value, setValue] = useState();

  /* Function for closing delete popup */
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  /* Function for opening snackbar */
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  /* Function for deleting a gst data */
  const deleteGst = () => {
    getDeleteGstByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleClosePopup = () => {
    setOpen(false);
  }

  return (
    <div>
      <div>
        <CustomLoader
          loading={getAllLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.topHeading}>
            <div className={Styles.heading}>
              <div className={Styles.subHeading}>
                <h3>GST</h3>
              </div>
            </div>
          </div>
          <div >
            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>GST Rate</th>
                  </tr>
                </thead>
                <tbody>
                  {getAllGstData?.map((data: any, index: number) => (
                    <tr key={data.gst_id}>
                      <td>{index + 1}</td>
                      <td>{data.rate}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
          {/* </div> */}
        </CustomLoader>
        <CustomDelete
          open={openDelete}
          title="Delete GST"
          contentLine1="Are you sure you want to delete this GST Data ?"
          contentLine2=""
          handleClose={handleCloseDelete}
          handleConfirm={deleteGst}
        />
        <CustomPopup
          open={open}
          handleClose={handleClosePopup}
          title={mode === "EDIT" ? "GST EDIT" : "NEW GST"}
          content={
            <GSTAddForm
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
