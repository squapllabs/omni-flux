import React from 'react';
import {
  IconButton,
  Tooltip,
  Link,
  Menu,
  MenuItem,
  Avatar,
} from '@mui/material';
import authService from '../service/auth-service';
import { useNavigate } from 'react-router-dom';
import Styles from '../styles/header.module.scss';
import LogoutIcon from '@mui/icons-material/Logout';
import { deepOrange } from '@mui/material/colors';
import { useDispatch } from 'react-redux';
import { resetAuth } from '../redux/reducer';
const Header = () => {
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const handleLogout = async () => {
    dispatch(resetAuth());
    const data = await authService.logout();
    if (data?.success === true) {
      navigate('/');
    }
  };
  const [anchorEl, setAnchorEl] = React.useState<null | HTMLElement>(null);

  const handleClick = (event: React.MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };
  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.app_name}>
          <span className={Styles.title}>Enterprise Application</span>
        </div>
        <div className={Styles.mainContent}>
          <ul className={Styles.navLinks}>
            <li>
              <Link href="/home" color="inherit">
                Home
              </Link>
            </li>
            <li>
              <Link color="inherit" onClick={handleClick}>
                Product & Solution
              </Link>
              <Menu
                id="sub-menu"
                className={Styles.subMenu}
                anchorEl={anchorEl}
                open={Boolean(anchorEl)}
                onClose={handleClose}
                MenuListProps={{
                  'aria-labelledby': 'sub-menu',
                }}
              >
                <MenuItem onClick={handleClose}>Cloud Computing</MenuItem>
                <MenuItem onClick={handleClose}>Security</MenuItem>
                <MenuItem onClick={handleClose}>Content Delivery</MenuItem>
              </Menu>
            </li>
            <li>
              <Link color="inherit">Resources</Link>
            </li>
            <li>
              <Link color="inherit">All Product</Link>
            </li>
            <li>
              <Link color="inherit">Contact Us</Link>
            </li>
          </ul>
        </div>
        <div className={Styles.loginprofile}>
          <Tooltip title="Profile">
            <IconButton>
              <Avatar sx={{ bgcolor: deepOrange[500], width: 35, height: 35 }}>
                N
              </Avatar>
            </IconButton>
          </Tooltip>
          <Tooltip title="Logout">
            <IconButton onClick={handleLogout}>
              <LogoutIcon className={Styles.logOutIcon} />
            </IconButton>
          </Tooltip>
        </div>
      </div>
    </div>
  );
};

export default Header;
