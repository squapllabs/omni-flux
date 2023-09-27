import React, { ReactNode } from 'react';
import styled from 'styled-components';

const CardWrapper = styled.div`
  background-color: #fff;
  border: 1px solid black;
  border-radius: 0px;
  padding: 20px;
  box-shadow: 0 2px 4px rgba(2,6, 2, 2);
`;

interface CustomCardProps {
    children: ReactNode;
  }
  
  const CustomCard: React.FC<CustomCardProps> = ({ children }) => {
    return <CardWrapper>{children}</CardWrapper>;
  };
  
  export default CustomCard;