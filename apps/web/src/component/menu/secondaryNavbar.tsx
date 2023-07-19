import React, { useState } from 'react';
import Button from './button';
import ToggleButton from './toggleButton';
import GreaterIcon from './icons/greaterthanIcon';
import Styles from '../../styles/navbar.module.scss';
import StarIcon from './icons/starIcon';
import DropdownIcon from './icons/dropDownButton';
import { Link } from 'react-router-dom';
import ToggleSwitch from './toggleButton';
const SecondaryNavbar = () => {
  const [activeLink, setActiveLink] = useState('Overview');
  const [toggleActive, setToggleActive] = useState(true);
  const handleLinkClick = (link: string) => {
    setActiveLink(link);
    console.log(`${link} clicked`);
  };

  const handleToggleSwitch = (checked: boolean) => {
    setToggleActive(checked);
    console.log(`Toggle Switch: ${checked}`);
  };
  return (
    <div className={Styles.Secondarynavbar}>
      <div className={Styles.container}>
        <div className={Styles.sequencesRow}>
          <h2>Sequences </h2>
          <GreaterIcon color="gray" />
        </div>
        <div className={Styles.sequenceNames}>Sequence name sho...</div>
      </div>
      <div className={Styles.actionDiv}>
        <nav className={Styles.SecondaryNaVlinks}>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Overview' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Overview')}
          >
            Overview
          </div>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Contacts' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Contacts')}
          >
            Contacts
          </div>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Emails' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Emails')}
          >
            Emails
          </div>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Tasks' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Tasks')}
          >
            Tasks
          </div>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Phone Calls' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Phone Calls')}
          >
            Phone Calls
          </div>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Activity Log' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Activity Log')}
          >
            Activity Log
          </div>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Setting' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Setting')}
          >
            Setting
          </div>
          <div
            className={`${Styles.navLink} ${
              activeLink === 'Report' ? Styles.activeLink : ''
            }`}
            onClick={() => handleLinkClick('Report')}
          >
            Report
          </div>
        </nav>
        <div className={Styles.Ctabuttons}>
          <div style={{ padding: '0 0 0 60px' }}>
            <StarIcon height={30} width={30} />
          </div>
          <div style={{ padding: '0 20px' }}>
            <Button
              text={
                <div>
                  More <DropdownIcon color="#1B92EB" />
                </div>
              }
              onClick={() => {}}
              backgroundColor="white"
              textColor="black"
              style={{ border: '1px solid grey' }}
              fontSize={16}
            />
          </div>
          <Button
            text={
              <div>
                Add Contacts <DropdownIcon color="white" />
              </div>
            }
            width="30%"
            fontSize={16}
            onClick={() => {}}
          />
          <div className={Styles.toggleButton}>
            <ToggleSwitch onChange={handleToggleSwitch} />
            <div
              className={Styles.toggleText}
              style={{ color: toggleActive ? '#02C697' : 'red' }}
            >
              {toggleActive ? 'Active' : 'Inactive'}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default SecondaryNavbar;
