import React, { useState, useRef, useEffect } from 'react';
import styled from 'styled-components';
import DropdownIcon from '../menu/icons/dropDownButton';
import CancelFilterIcon from '../menu/icons/cancelFilterIcon';
import CloseIcon from '../menu/icons/closeIcon';
import ClearIcon from '../menu/icons/closeIcon';
import AddIcon from '../menu/icons/addIcon';

interface InputWrapperProps {
  width?: string;
}

interface StyledInputProps {
  error?: boolean;
  hasPrefixIcon?: boolean;
  hasSuffixIcon?: boolean;
  transparent?: boolean;
  disabled?: boolean;
}

interface Option {
  value: number;
  label: string;
}

interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label?: string;
  placeholder?: string;
  error?: string;
  width?: string;
  prefixIcon?: React.ReactNode;
  suffixIcon?: React.ReactNode;
  transparent?: boolean;
  onChange?: (e: React.ChangeEvent<HTMLInputElement>) => void;
  value?: string;
  selectedValue?: string;
  onSelect: (e: string) => void;
  optionList: Option[];
  defaultLabel: string;
  addLabel: string;
  onAddClick: (e: string) => void;
}



const OptionContainer = styled.div`
  position: relative;
  display: inline-block;
  width: 100%;
`;

const OptionList = styled.ul`
  position: absolute;
  top: 100%;
  left: 0;
  right: 0;
  z-index: 1;
  background-color: #fff;
  border: 1px solid #ccc;
  border-radius: 4px;
  box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2);
  max-height: 200px;
  overflow-y: auto;
  padding: 0;
  margin: 0;
  list-style: none;
  li {
    padding: 8px 12px;
    cursor: pointer;
    &:hover {
      background-color: #f4f5f6;
    }
  }
`;

const InputWrapper = styled.div<InputWrapperProps>`
  display: flex;
  flex-direction: column;
  width: ${(props) => props.width || '100%'};
`;

const StyledLabel = styled.label`
  margin-bottom: 4px;
  font-size: 0.8rem;
  color: #333c44;
  font-weight: 600;
`;

const InputContainer = styled.div<StyledInputProps>`
  position: relative;
  display: flex;
  align-items: center;
  padding: ${(props) =>
    `0 ${props.hasSuffixIcon ? '32px' : '12px'} 0 ${props.hasPrefixIcon ? '32px' : '12px'
    }`};
  border: 1px solid ${(props) => (props.error ? 'red' : '#ccc')};
  border-radius: 4px;
  background-color: ${(props) =>
    props.transparent ? 'transparent' : '#f4f5f6'};
  cursor: ${(props) => (props.disabled ? 'not-allowed' : 'pointer')};
  opacity: ${(props) => (props.disabled ? 0.7 : 1)};
  &:hover {
    border-color: ${(props) => (props.disabled ? '#ccc' : '#888')};
    cursor: ${(props) => (props.disabled ? 'not-allowed' : 'pointer')};
  }
  &:focus-within {
    outline: 0;
    box-shadow: ${(props) => (props.disabled ? 'none' : '0 0 0 2px #68717840')};
  }
`;

const StyledInput = styled.input<StyledInputProps>`
  height: 34px;
  padding: ${(props) => `6px ${props.hasSuffixIcon ? '32px' : '0'} 6px 0`};
  border: none;

  background-color: ${(props) => (props.disabled ? '#f9f9f9' : 'transparent')};
  pointer-events: ${(props) => (props.disabled ? 'none' : 'auto')};
  color: ${(props) => (props.disabled ? '#888' : 'inherit')};
  flex-grow: 1;
  &:focus {
    outline: none;
  }
  box-sizing: border-box;
`;

const IconWrapper = styled.div`
  display: flex;
`;

const PrefixIconWrapper = styled(IconWrapper)`
  position: absolute;
  left: 8px;
`;

const SuffixIconWrapper = styled(IconWrapper)`
  position: absolute;
  right: 8px;
`;

const InputError = styled.span`
  color: red;
  margin-top: 2px;
  font-size: 0.75rem;
`;

const RequiredField = styled.span`
  color: red;
`;

const ErrorMessageWrapper = styled.div`
  min-height: 20px;
`;


const AutoCompleteMultiSelect: React.FC<InputProps & { mandatory?: boolean }> = ({
    
  label,
  placeholder,
  error,
  width,
  prefixIcon,
  suffixIcon,
  transparent,
  disabled,
  value,
  selectedValue,
  mandatory = false,
  onSelect,
  optionList,
  defaultLabel,
  addLabel,
  onAddClick,
  ...props
}) => {
  const shouldShowAsterisk = mandatory; // You can add logic for this if needed

  const [filteredOptions, setFilteredOptions] = useState<Option[]>(optionList);
  const [selectedValues, setSelectedValues] = useState<string[]>([]);
  const [values, setValues] = useState('');
  const [inputValue, setInputValue] = useState('');
  const [combinedValue, setCombinedValue] = useState<string>('');
  const [open, setOpen] = useState(false);
  const [selectedOptions,setSelectedOptions]=useState<string[]>([]);


  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const inputValue = e.target.value.toLowerCase();
    setValues(inputValue);
    const filtered = optionList.filter((option) =>
      option.label.toLowerCase().includes(inputValue)

    );
    setFilteredOptions(filtered);
  };


  const handleBackspace = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Backspace' && inputValue === '') {
      // Remove the last selected value
      const updatedSelectedValues = [...selectedValues];
      updatedSelectedValues.pop(); // Remove the last value
      setSelectedValues(updatedSelectedValues);

      // Update the combined value for display
      const newCombinedValue = updatedSelectedValues.join(', ');
      setCombinedValue(newCombinedValue);
    }
  };



useEffect(() => {
    const handleOutsideClick = (event: MouseEvent) => {
  
      if ( inputRef.current && !inputRef.current.contains(event.target as Node)) {
        setOpen(false);
      }
    };
    document.addEventListener('click', handleOutsideClick);
  
    return () => {
      document.removeEventListener('click', handleOutsideClick);
    };
  }, [open, selectedValues,selectedOptions]);
  
  

  const handleSelect = (option: Option) => {
    if (!selectedValues.includes(option.label)) {
        setSelectedOptions([...selectedOptions,option.label])
      setSelectedValues([...selectedValues, option]);
      onSelect([...selectedValues,option])
    }
  };

// const handleSelect = (option: Option) => {
//     if (!selectedValues.some((selected) => selected.value === option.value)) {
//         setSelectedValues([...selectedValues, option]);
//     }
//   };

  const handleDeselect = (label: string,option:string) => {
    const updatedOptions = selectedOptions.filter((value) => value !== label);
    const updatedValues = selectedValues.filter((option) => option.label !== label);
    setSelectedOptions(updatedOptions);
    setSelectedValues(updatedValues);
    onSelect(updatedValues);
  };

  useEffect(() => {
    setFilteredOptions(optionList);
  }, [optionList]);

  const inputRef = useRef<HTMLInputElement | null>(null);



  return (
    <InputWrapper width={width}>
      {label && (
        <StyledLabel>
          {label} {shouldShowAsterisk && <RequiredField>*</RequiredField>}{' '}
        </StyledLabel>
      )}
      <InputContainer
        error={!!error}
        hasPrefixIcon={!!prefixIcon}
        hasSuffixIcon={!!suffixIcon}
        transparent={transparent}
        disabled={disabled}
      >
        {prefixIcon && <PrefixIconWrapper>{prefixIcon}</PrefixIconWrapper>}
        <StyledInput
          ref={inputRef}
          hasSuffixIcon={!!suffixIcon}
          placeholder={placeholder}
          disabled={disabled}
          readOnly
          value={selectedOptions}
          onKeyDown={(e) => handleBackspace(e)}
          {...props}
          onChange={(e) => handleChange(e)}
          onFocus={() => {
            setOpen(true);
          }}
        />
        <SuffixIconWrapper>
        <div
          style={{
            display: 'flex',
            flexDirection: 'column', // Display clear icons below each value
            alignItems: 'flex-end', // Align clear icons to the right
          }}
        >
          <div
            onClick={(e) => {
              e.stopPropagation();
              setOpen(!open);
            }}
          >
            <DropdownIcon/>
          </div>
        </div>
      </SuffixIconWrapper>
      </InputContainer>

      {open && (
        <OptionContainer>
          <OptionList >
            {defaultLabel != null && (
              <li value="">{defaultLabel}</li>
            )}
            {filteredOptions.map((option) => (
              <li
                key={option.value}
                onClick={() => {
                  if (selectedOptions.includes(option.label)) {
                    // If the option is already selected, deselect it
                    handleDeselect(option.label,option);
                    // handleDeselect(option)
                  } else {
                    // If the option is not selected, select it
                    handleSelect(option);
                    // selectedOptions(option.label)
                    // onSelect(option);
                  }
                }}
                style={{
                  backgroundColor: selectedOptions.includes(option.label)
                    ? '#EFF5F5'
                    : '',
                }}
              >
                {option.label}
              </li>
            ))}
            {addLabel != null && (
              <li
                value="add"
                onClick={() => {
                  setOpen(!open);
                  onAddClick(value);
                }}
              >
                <div
                  style={{
                    display: 'flex',
                    alignItems: 'center',
                    gap: '10px',
                    color: '#7f56d9',
                    fontSize: '12px',
                  }}
                >
                  <AddIcon color="#7f56d9" width={15} />
                  {addLabel}
                </div>
              </li>
            )}
          </OptionList>
        </OptionContainer>
      )}
     
      {error && (
        <ErrorMessageWrapper>
          {error && <InputError>{error}</InputError>}
        </ErrorMessageWrapper>
      )}
    </InputWrapper>
  );
}


export default AutoCompleteMultiSelect;