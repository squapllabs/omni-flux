import React, { useEffect, useRef, useState } from 'react';
import styled from 'styled-components';
import { FaChevronDown } from "react-icons/fa";
import debounce from '@mui/material/utils/debounce';


interface Option {
  value: string;
  label: string;
}

interface CustomAutoDropdownProps {
  value: string;
  onChange: (value: string) => void;
  getOptionsFromAPI: (input: string) => Promise<Option[]>;
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

const DropdownListContainer = styled.div<{ maxHeight: string | undefined }>`
  position: absolute;
  width: 100%;
  z-index: 2000;
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #fff;
  max-height: ${(props) => props.maxHeight || "auto"};
  overflow: auto;
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

const SearchInput = styled.input`
  width: 100%;
  padding: 10px;
  border: 1px solid #ccc;
  outline: none;
`;

const CustomAutoDropdown: React.FC<CustomAutoDropdownProps> = ({
  value,
  onChange,
  getOptionsFromAPI,
  defaultLabel = "Select an option",
  width,
  maxHeight,
  required = false
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const [options, setOptions] = useState<Option[]>([]);
  const [searchTerm, setSearchTerm] = useState<string>("");
  const ref = useRef<HTMLDivElement>(null);

  const fetchOptions = debounce(async (input: string) => {
        
    const results = await getOptionsFromAPI(input);
    setOptions(results);
  }, 500);

  useEffect(() => {
    if(searchTerm !== '') {
      fetchOptions(searchTerm);
    }
  }, [searchTerm]);

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

  // const headerText = value || defaultLabel;
  const headerText = value ? options.find(option => option.value === value)?.label || defaultLabel : defaultLabel;


  return (
    <DropdownContainer ref={ref} width={width}>
      <DropdownHeader onClick={toggling}>
        <span>{headerText}</span>
        <FaChevronDown size={20}/>
      </DropdownHeader>
      {isOpen && (
        <DropdownListContainer maxHeight={maxHeight}>
          <SearchInput
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
            placeholder="Start typing..."
          />
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

export default CustomAutoDropdown;