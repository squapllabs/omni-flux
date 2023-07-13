import React, { useState } from 'react';
import Styles from '../../styles/userList.module.scss';
import MUIDataTable from 'mui-datatables';
import AddCircleOutlinedIcon from '@mui/icons-material/AddCircleOutlined';
import {useGetAllUsers,useDeleteUsers,useGetAllInactiveUsers} from '../../hooks/user-hooks';
import { useNavigate } from 'react-router';
import { Button } from '@mui/material';
import { Tooltip, IconButton } from '@mui/material';
import CustomDialog from '../ui/customDialog';
import MySnackbar from '../ui/MySnackbar';
// import MoreVertIcon from '@mui/icons-material/MoreHoriz';
import Card from '@mui/material/Card';
import AppBar from '@mui/material/AppBar';
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import VisibilityIcon from '@mui/icons-material/Visibility';

function TabPanel(props: any) {
  const { children, value, index, ...other } = props;
  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`simple-tabpanel-${index}`}
      aria-labelledby={`simple-tab-${index}`}
      {...other}
    >
      {value === index && <Box p={2}>{children}</Box>}
    </div>
  );
}

const UserList = () => {
  const { data: getAllUsers, isLoading: loader } = useGetAllUsers();
  const { data: getAllInactiveUsers, isLoading: loaderOne } = useGetAllInactiveUsers();
  const { mutate: getDeleteUserByID } = useDeleteUsers();
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');
  const [tabvalue, setTabValue] = useState(0);
  const [reloadActive, setReloadActive] = useState(false);
  const [reloadInactive, setReloadInactive] = useState(false);
  const navigate = useNavigate();

  const deleteUserHandler = (id: any) => {
    setValue(id);
    setOpen(true);
  };
  const handleClose = () => {
    setOpen(false);
  };
  const handleSnackBarClose = () => {
    setOpenDeleteSnack(false);
  };

  const deleteUser = () => {
    getDeleteUserByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenDeleteSnack(true);
  };


  const handleChangeTab = (event: any, newValue: any) => {
    setTabValue(newValue);
  };

  function a11yProps(index: any) {
    return {
      id: `simple-tab-${index}`,
      'aria-controls': `simple-tabpanel-${index}`,
    };
  }

  const handleTab = (index_value: any) => {
    if (index_value === 0) {
      console.log("gggg");
      setReloadActive(!reloadActive);
    } else if (index_value === 1) {
      setReloadInactive(!reloadInactive);
    }
  };

  const columns = [
    {
      name: 'user_id',
      label: 'User',
      options: {
        display: false,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'S No',
      label: 'S No',
      options: {
        display: true,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return tableMeta.rowIndex + 1;
        },
      },
    },

    {
      name: 'first_name',
      label: 'First Name',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'last_name',
      label: 'Last Name',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'email_id',
      label: 'Email',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'contact_no',
      label: 'Contact Number',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
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
              <Tooltip title="Edit">
                <IconButton
                  aria-label="Edit"
                  size="small"
                  onClick={() => navigate(`/user-edit/${tableMeta.rowData[0]}`)}
                >
                  <EditIcon />
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
    selectableRows: 'none' as const,
    textLabels: {
      body: {
        noMatch: loader ? 'Loading...' : 'Sorry , No Records found',
      },
    },
    setTableProps: () => {
      return {
        size: 'small',
      };
    },
  };

  const columnsOne = [
    {
      name: 'user_id',
      label: 'User',
      options: {
        display: false,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'S No',
      label: 'S No',
      options: {
        display: true,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return tableMeta.rowIndex + 1;
        },
      },
    },

    {
      name: 'first_name',
      label: 'First Name',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'last_name',
      label: 'Last Name',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'email_id',
      label: 'Email',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'contact_no',
      label: 'Contact Number',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
  ];

  const optionsOne = {
    filter: false,
    search: true,
    caseSensitive: false,
    print: false,
    download: false,
    viewColumns: false,
    selectableRows: 'none' as const,
    textLabels: {
      body: {
        noMatch: loaderOne ? 'Loading...' : 'Sorry , No Records found',
      },
    },
    setTableProps: () => {
      return {
        size: 'small',
      };
    },
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.buttonContainer}>
        <Button
          variant="contained"
          color="primary"
          startIcon={<AddCircleOutlinedIcon />}
          onClick={() => navigate('/user-create')}
        >
          Add
        </Button>
      </div>
      <div className={Styles.tableContainer}>
        <Card>
          <AppBar position="static">
            <Tabs
              value={value}
              onChange={handleChangeTab}
              variant="fullWidth"
              indicatorColor="primary"
              style={{
                background: 'white',
              }}
            >
              <Tab
                style={{
                  fontWeight: 'bold',
                }}
                label="Active Users"
                onClick={(e) => handleTab(0)}
                {...a11yProps(0)}
              />
              <Tab
                style={{
                  fontWeight: 'bold',
                }}
                label="Inactive Users"
                onClick={(e) => handleTab(1)}
                {...a11yProps(1)}
              />
            </Tabs>
          </AppBar>
          <TabPanel value={tabvalue} index={0}>
            <MUIDataTable
              key={`active-${reloadActive}`}
              title={`Users List (${getAllUsers?.count})`}
              data={getAllUsers?.data}
              columns={columns}
              options={options}
            />
          </TabPanel>

          <TabPanel value={tabvalue} index={1}>
            <MUIDataTable
              key={`inactive-${reloadInactive}`}
              title={`Inactive User List (${getAllInactiveUsers?.count})`}
              data={getAllInactiveUsers?.data}
              columns={columnsOne}
              options={optionsOne}
            />
          </TabPanel>
        </Card>
      </div>
      <CustomDialog
        open={open}
        handleClose={handleClose}
        title="Delete User"
        content="Are you want to delete this User?"
        handleConfirm={deleteUser}
      />
      <MySnackbar
        open={openDeleteSnack}
        message={message}
        onClose={handleSnackBarClose}
        severity={'success'}
        autoHideDuration={1000}
      />
    </div>
  );
};

export default UserList;
