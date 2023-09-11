import React from 'react';
import Button from '../../ui/Button';

const ProjectSiteConfig: React.FC = (props: any) => {
  return (
    <div>
      <div>Site Config</div>
      <Button
        shape="rectangle"
        justify="center"
        size="small"
        onClick={(e) => props.setActiveButton('PBC')}
      >
        SAVE & MOVE ON
      </Button>
    </div>
  );
};

export default ProjectSiteConfig;
