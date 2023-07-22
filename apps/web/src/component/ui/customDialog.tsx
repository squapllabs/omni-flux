import React, { useState, useRef } from 'react';
import styled from 'styled-components';
import { FaChevronDown } from "react-icons/fa";

interface Option {
  value: string;
  label: string;
}

interface CustomDropdownProps {
  options: Option[];
  value: string;
  onChange: (value: string) => void;
  defaultLabel?: string;
  width?: string;
  maxHeight?: string;
}

const DropdownContainer = styled.div<{ width: string | undefined }>`
  position: relative;
  width: ${(props) => props.width || "auto"};
`;

const DropdownHeader = styled.div`
  padding: 10px;
  color: #333;
  background-color: #f4f5f6;
  border-radius: 4px;
  cursor: pointer;
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const DropdownListContainer = styled.div<{ maxHeight: string | undefined }>`
  position: absolute;
  width: 100%;
  z-index: 2000; // Increased z-index
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #fff;
  max-height: ${(props) => props.maxHeight || "auto"};
  overflow: auto;
`;

const DropdownList = styled.ul`
  padding: 0;
  margin: 0;
  padding-left: 1em;
  list-style: none;
`;

const ListItem = styled.li<{ isSelected: boolean }>`
  list-style: none;
  padding: 20px 10px; // Increased padding
  background: ${(props) => (props.isSelected ? '#f4f5f6' : '#fff')};
  &:hover {
    cursor: pointer;
    background-color: #f4f5f6;
  }
`;

const CustomDropdown: React.FC<CustomDropdownProps> = ({
  options,
  value,
  onChange,
  defaultLabel = "Select from options",
  width,
  maxHeight
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const toggling = () => setIsOpen(!isOpen);
  const onOptionClicked = (value: string) => () => {
    onChange(value);
    setIsOpen(false);
  };

  return (
    <DropdownContainer width={width}>
      <DropdownHeader onClick={toggling}>
        <span>{value || defaultLabel}</span>
        <FaChevronDown size={20}/>  // You can adjust the size of the down arrow here
      </DropdownHeader>
      {isOpen && (
        <DropdownListContainer maxHeight={maxHeight}>
          <DropdownList>
            {options.map(option => (
              <ListItem
                key={option.value}
                onClick={onOptionClicked(option.value)}
                isSelected={value === option.value}
              >
                {option.label}
              </ListItem>
            ))}
          </DropdownList>
        </DropdownListContainer>
      )}
    </DropdownContainer>
  );
};

export default CustomDropdown;
