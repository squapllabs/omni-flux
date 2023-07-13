import React, { useState } from 'react';
import Styles from '../../styles/userList.module.scss';
import MUIDataTable from 'mui-datatables';
import AddCircleOutlinedIcon from '@mui/icons-material/AddCircleOutlined';
import { useGetAllUsers, useDeleteUsers } from '../../hooks/user-hooks';
import { useNavigate } from 'react-router';
import { Button } from '@mui/material';
import { Tooltip, IconButton, Menu, MenuItem } from '@mui/material';
// import DeleteIcon from '@mui/icons-material/Delete';
// import EditIcon from '@mui/icons-material/Edit';
import CustomDialog from '../ui/customDialog';
// import VisibilityIcon from '@mui/icons-material/Visibility';
import MySnackbar from '../ui/MySnackbar';
import MoreVertIcon from '@mui/icons-material/MoreHoriz';
import Card from '@mui/material/Card';
import AppBar from '@mui/material/AppBar';
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';

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
  const { mutate: getDeleteUserByID } = useDeleteUsers();
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [anchorEl, setAnchorEl] = useState(null);
  const [tabvalue, setTabValue] = useState(0);
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
    setAnchorEl(null);
    setMessage('Successfully deleted');
    setOpenDeleteSnack(true);
  };

  const handleMenuClick = (event: any) => {
    setAnchorEl(event.currentTarget);
  };

  const handleMenuClose = () => {
    setAnchorEl(null);
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

  const renderOptionsMenu = (id: any) => (
    <Menu
      id={`options-menu-${id}`}
      anchorEl={anchorEl}
      open={Boolean(anchorEl)}
      onClose={handleMenuClose}
    >
      <MenuItem onClick={() => navigate(`/userInfo/${id}`)}>View</MenuItem>
      <MenuItem onClick={() => deleteUserHandler(id)}>Delete</MenuItem>
      <MenuItem onClick={() => navigate(`/user-edit/${id}`)}>Edit</MenuItem>
    </Menu>
  );
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
          const userId = tableMeta.rowData[0];
          return (
            <div>
              <Tooltip title="Options">
                <IconButton
                  aria-label="Options"
                  size="small"
                  onClick={handleMenuClick}
                >
                  <MoreVertIcon />
                </IconButton>
              </Tooltip>
              {renderOptionsMenu(userId)}
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

  const request = [{
    user_id:"2",
    first_name:"kumar",
    last_name:"ki",
    email_id:"tharane.tha",
    contact_no:"456789"
  }]

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
    }
  ]

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
        noMatch: loader ? 'Loading...' : 'Sorry , No Records found',
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
              TabIndicatorProps={{
                style: {
                  background: 'red',
                  color: 'blue',
                },
              }}
            >
              <Tab
                style={{
                  fontWeight: 'bold',
                }}
                label="Active Users"
                {...a11yProps(0)}
              />
              <Tab
                style={{
                  fontWeight: 'bold',
                }}
                label="Inactive Users"
                {...a11yProps(1)}
              />
            </Tabs>
          </AppBar>
          <TabPanel value={tabvalue} index={0}>
            <MUIDataTable
              title={`Users List (${
                getAllUsers?.length ? getAllUsers?.length : 0
              })`}
              data={getAllUsers}
              columns={columns}
              options={options}
            />
          </TabPanel>

          <TabPanel value={tabvalue} index={1}>
            <div>sample</div>
            <MUIDataTable
              title={`Inactive User List (${
                request?.length ? request?.length : 0
              })`}
              data={request}
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
