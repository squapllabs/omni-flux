import { useGetByuserID } from '../../hooks/user-hooks';
import Styles from '../../styles/userInfo.module.scss';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router';
import Card from '@mui/material/Card';
import Button from '../menu/button';

const UserInformation = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const userId = Number(routeParams?.id);
  const { data: getOneUser } = useGetByuserID(userId);

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
              <div className={Styles.leftData}>Department</div>
              <div className={Styles.leftData}>Gender</div>
              <div className={Styles.leftData}>Address</div>
            </div>
            <div className={Styles.rightContent}>
              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userData?.first_name
                  ? `${getOneUser?.userData?.first_name} ${
                      getOneUser?.userData?.last_name
                        ? getOneUser?.userData?.last_name
                        : ''
                    }`
                  : 'Not Provided'}
              </div>
              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userData?.email_id
                  ? getOneUser?.userData?.email_id
                  : 'Not Provided'}
              </div>
              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userData?.contact_no
                  ? getOneUser?.userData?.contact_no
                  : 'Not Provided'}
              </div>

              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userData?.department
                  ? getOneUser?.userData?.department
                  : 'Not Provided'}
              </div>
              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userProfileData?.gender
                  ? getOneUser?.userProfileData?.gender
                  : 'Not Provided'}
              </div>
              <div className={Styles.rightData}>
                :{' '}
                {getOneUser?.userProfileData?.address ? (
                  <>
                    {getOneUser?.userProfileData?.address.street && (
                      <>{getOneUser?.userProfileData?.address.street}, </>
                    )}
                    {getOneUser?.userProfileData?.address.city && (
                      <>{getOneUser?.userProfileData?.address.city}, </>
                    )}
                    {getOneUser?.userProfileData?.address.state && (
                      <>{getOneUser?.userProfileData?.address.state}, </>
                    )}
                    {getOneUser?.userProfileData?.address.country && (
                      <>{getOneUser?.userProfileData?.address.country}, </>
                    )}
                    {getOneUser?.userProfileData?.address.pin_code}
                  </>
                ) : (
                  'Address not available'
                )}
              </div>
            </div>
          </div>
        </Card>
        <div className={Styles.buttonContainer}>
          <Button
            text="Back"
            backgroundColor="#7F56D9"
            fontSize={14}
            fontWeight={500}
            width={100}
            onClick={() => navigate('/userList')}
          />
        </div>
      </div>
    </div>
  );
};

export default UserInformation;
