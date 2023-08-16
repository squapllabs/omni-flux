import React, { useState } from "react";
import Styles from '../../styles/homeSettings.module.scss';
import CustomGroupButton from "../ui/CustomGroupButton";
import Category from "../category/category";
import UserList from "../users/userList";
import MasterData from "../masterData/masterData";
import ProjectList from "../project/projectList";
const Settings = () => {
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Category', value: 'C' },
    { label: 'Master Data', value: 'M' },
    { label: 'User List', value: 'U' },
    { label: 'Project List', value: 'PL' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('C');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  }
  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.heading}>Settings</div>
        <div className={Styles.button}>
          <CustomGroupButton
            labels={buttonLabels}
            onClick={handleGroupButtonClick}
            activeButton={activeButton}
          />
        </div>
      </div>
      <div className={Styles.dividerLine}></div>
      <div>
        {activeButton === 'C' && <Category />}
        {activeButton === 'M' && <MasterData />}
        {activeButton === 'U' && <UserList />}
        {activeButton === 'PL' && <ProjectList />}
      </div>
    </div>
  )
}
export default Settings;