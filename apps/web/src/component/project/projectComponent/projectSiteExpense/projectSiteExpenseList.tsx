import React, { useState } from 'react';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import CustomSidePopup from '../../../ui/CustomSidePopup';
import ProjectSiteExpenseForm from './projectSiteExpenseForm';
import { useNavigate, useParams } from 'react-router-dom';

const ProjectSiteExpenseList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const [open, setOpen] = useState(false);
  const handleClose = () => {
    setOpen(false);
  };
  return (
    <div>
      <div>
        <Button
          type="button"
          color="primary"
          shape="rectangle"
          size="small"
          justify="center"
          icon={<AddIcon width={20} color="white" />}
          onClick={(e) => {
            setOpen(true);
          }}
        >
          Add
        </Button>
      </div>
      <CustomSidePopup
        open={open}
        handleClose={handleClose}
        title={'Add Site Expense'}
        content={<ProjectSiteExpenseForm />}
      />
    </div>
  );
};

export default ProjectSiteExpenseList;
