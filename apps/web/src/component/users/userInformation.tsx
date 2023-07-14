import { getByuserID } from '../../hooks/user-hooks';
import Styles from '../../styles/userInfo.module.scss';
import { useParams } from 'react-router-dom';
import { Button } from '@mui/material';
import { useNavigate } from 'react-router';
import Card from '@mui/material/Card';

const UserInformation = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const userId = Number(routeParams?.id);
  const { data: getOneUser } = getByuserID(userId);
  
  return (
    <div>
      <div className={Styles.mainContainer}>
        <div className={Styles.title}>
          <h2>User Information</h2>
        </div>
        <Card className={Styles.cardContent}>
          <div className={Styles.mainContent}>
            <div className={Styles.leftContent}>
              <div className={Styles.leftData}>User Name</div>
              <div className={Styles.leftData}>Email</div>
              <div className={Styles.leftData}>Contact Number</div>
              <div className={Styles.leftData}>Address</div>
            </div>
            <div className={Styles.rightContent}>
              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userData?.first_name
                  ? `${getOneUser?.userData?.first_name} ${
                      getOneUser?.userData?.last_name ? getOneUser?.userData?.last_name : ''
                    }`
                  : 'Not Provided'}
              </div>
              <div className={Styles.rightData}>
                : {getOneUser?.userData?.email_id ? getOneUser?.userData?.email_id : 'Not Provided'}
              </div>
              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userData?.contact_no
                  ? getOneUser?.userData?.contact_no
                  : 'Not Provided'}
              </div>
              <div className={Styles.rightData}>
                : {getOneUser?.userData?.address ? getOneUser?.userData?.address : 'Not Provided'}
              </div>
            </div>
          </div>
        </Card>
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
    </div>
  );
};

export default UserInformation;