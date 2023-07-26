import React, { useEffect, useRef, useState } from 'react';
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
  required?: boolean;
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

interface DropdownListContainerProps {
  maxHeight: string | undefined;
  dropdownDirection: string;
}

const DropdownListContainer = styled.div<DropdownListContainerProps>`
  position: absolute;
  width: 100%;
  z-index: 2000;
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #fff;
  max-height: ${(props) => props.maxHeight || '200px'};
  overflow: auto;
  top: ${(props) => (props.dropdownDirection === 'down' ? '100%' : 'unset')};
  bottom: ${(props) => (props.dropdownDirection === 'up' ? '100%' : 'unset')};
`;

const DropdownList = styled.ul`
  padding: 0;
  margin: 0;
`;

const ListItem = styled.li<{ isSelected: boolean }>`
  list-style: none;
  padding: 10px;
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
  defaultLabel = "Select an option",
  width,
  maxHeight,
  required = false
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const [dropdownDirection, setDropdownDirection] = useState('down');
  const ref = useRef<HTMLDivElement>(null);

  const toggling = () => setIsOpen(!isOpen);

  const onOptionClicked = (value: string) => () => {
    onChange(value);
    setIsOpen(false);
  };

  useEffect(() => {
    const onDocumentClick = (event: MouseEvent) => {
      if (ref.current && !ref.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };

    if (isOpen) {
      document.addEventListener("click", onDocumentClick);
    }

    return () => {
      document.removeEventListener("click", onDocumentClick);
    };
  }, [isOpen]);

  useEffect(() => {
    if (!ref.current) return;
    const rect = ref.current.getBoundingClientRect();
    const spaceUp = rect.top;
    const spaceDown = window.innerHeight - rect.bottom;

    setDropdownDirection(spaceDown > spaceUp ? 'down' : 'up');
  }, []);

  const headerText = value || defaultLabel;

  return (
    <DropdownContainer ref={ref} width={width}>
      <DropdownHeader onClick={toggling}>
        <span>{headerText}</span>
        <FaChevronDown size={20}/>
      </DropdownHeader>
      {isOpen && (
        <DropdownListContainer dropdownDirection={dropdownDirection} maxHeight={maxHeight}>
          <DropdownList>
            <ListItem
              key="default"
              onClick={onOptionClicked('')}
              isSelected={!value}
            >
              {defaultLabel}
            </ListItem>
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