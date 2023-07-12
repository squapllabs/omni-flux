import React, { useState } from 'react';
import Layout from '../../layout/layout';
import Styles from '../../styles/userList.module.scss';
import MUIDataTable from 'mui-datatables';
import AddCircleOutlinedIcon from '@mui/icons-material/AddCircleOutlined';
import { useGetAllUsers } from '../../hooks/user-hooks';
import { useNavigate } from 'react-router';
import { Button } from '@mui/material';
import { Tooltip, IconButton } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import CustomDialog from '../ui/customDialog';
import VisibilityIcon from '@mui/icons-material/Visibility';

const UserList = () => {
  const { data: getAllUsers } = useGetAllUsers();
  const [open, setOpen] = useState(false);
  const navigate = useNavigate();

  const deleteUserHandler = (id: number) => {
    setOpen(true);
    console.log('delete id', id);
  };
  const handleClose = () => {
    setOpen(false);
  };

  const deleteUser = () => {
    console.log('sample');
  };

  const columns = [
    {
      name: 'user_id',
      label: 'User',
      options: {
        display: false,
        filter: false,
        sort: false,
      }
    },
    {
      name: 'first_name',
      label: 'First Name',
      options: {
        display: true,
        filter: false,
        sort: false,
      }
    },
    {
      name: 'last_name',
      label: 'Last Name',
      options: {
        display: true,
        filter: false,
        sort: false,
      }
    },
    {
      name: 'email_id',
      label: 'Email',
      options: {
        display: true,
        filter: false,
        sort: false,
      }
    },
    {
      name: 'contact_no',
      label: 'Contact Number',
      options: {
        display: true,
        filter: false,
        sort: false,
      }
    },
    {
      name: '',
      label: 'Options',
      options: {
        sort: false,
        filter: false,
        searchable: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return (
            <div>
              <Tooltip title="View">
                <IconButton
                  aria-label="View"
                  size="small"
                  onClick={() => navigate(`/userInfo/${tableMeta.rowData[0]}`)}
                >
                  <VisibilityIcon />
                </IconButton>
              </Tooltip>
              <Tooltip title="Delete">
                <IconButton
                  aria-label="Delete"
                  size="small"
                  onClick={() => deleteUserHandler(tableMeta.rowData[0])}
                >
                  <DeleteIcon />
                </IconButton>
              </Tooltip>
            </div>
          );
        },
      },
    },
  ];

  const options = {
    filter: false,
    search: true,
    caseSensitive: false,
    print: false,
    download: false,
    viewColumns: false,
    selectableRows: 'none',
    setTableProps: () => {
      return {
        size: 'small',
      };
    },
  };
  return (
    <div className = {Styles.container}>
      <Layout />
      <div className={Styles.buttonContainer}>
        <Button
          variant="contained"
          color="primary"
          startIcon={<AddCircleOutlinedIcon />}
          onClick={() => navigate('/home')}
        >
          Add
        </Button>
      </div>
      <div className={Styles.tableContainer}>
        <MUIDataTable
          title={'User List'}
          columns={columns}
          options={options}
          data={getAllUsers}
        />
      </div>
      <CustomDialog
        open={open}
        handleClose={handleClose}
        title="Delete User"
        content="Are you want to delete this User?"
        handleConfirm={deleteUser}
      />
    </div>
  );
};

export default UserList;
