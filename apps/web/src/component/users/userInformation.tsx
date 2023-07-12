import Layout from '../../layout/layout';
import { getByuserID } from '../../hooks/user-hooks';
import Styles from '../../styles/userInfo.module.scss';
import { useParams } from 'react-router-dom';
import { Button } from '@mui/material';
import { useNavigate } from 'react-router';

const UserInformation = () => {

  const routeParams = useParams();
  console.log('routeParams ===>', routeParams);
  const navigate = useNavigate();
  const userId = Number(routeParams?.id);
  const { data: getOneUser } = getByuserID(userId);
  console.log('user info===>', getOneUser);
  return (
    <div>
      <Layout />
      <div className={Styles.title}>
        <h2>User Information</h2>
      </div>
      <div className={Styles.mainContent}>
        <div className={Styles.leftContent}>
          <div className={Styles.leftData}>User Name</div>
          <div className={Styles.leftData}>Email</div>
          <div className={Styles.leftData}>Contact Number</div>
          <div className={Styles.leftData}>Address</div>
        </div>
        <div className={Styles.rightContent}>
          <div className={Styles.rightData}>
            : {getOneUser?.first_name ? getOneUser?.first_name : 'Not Provided'}
          </div>
          <div className={Styles.rightData}>
            : {getOneUser?.email_id ? getOneUser?.email_id : 'Not Provided'}
          </div>
          <div className={Styles.rightData}>
            : {getOneUser?.contact_no ? getOneUser?.contact_no : 'Not Provided'}
          </div>
          <div className={Styles.rightData}>
            : {getOneUser?.Address ? getOneUser?.Address : 'Not Provided'}
          </div>
        </div>
      </div>
      <div className={Styles.buttonContainer}> 
      <Button
          variant="contained"
          color="primary"
          onClick={() => navigate('/userList')}
        >
          Back
        </Button>
      </div>
    </div>
  );
};

export default UserInformation;
