import React, { useState, useRef, useEffect } from 'react';
import styled from 'styled-components';
import DropdownIcon from '../menu/icons/dropDownButton';
import CancelFilterIcon from '../menu/icons/cancelFilterIcon';
import CloseIcon from '../menu/icons/closeIcon';

// Docs:
// Should pass optionList as an array of objects with id and name
// Should pass onSelect as a function that sets the value to the selected option
// Should pass value as a string
// Should pass onChange as a function that sets the value to the input value
// Example:
// const [value, setValue] = useState('')
// const optionList = [{id:1, label:'Option1'}, {id:2, label:'Option2'}, {id:3, label:'Option3'}, {id:4, label:'Option4'}]
// const handleValueChange = (e: React.ChangeEvent<HTMLInputElement>) => {
//     setValue(e.target.value)
// }
// const handleSelect = (option: string) => {
//     setValue(option)
// }

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
  onSelect: (e: string) => void;
  optionList: Option[];
  defaultLabel: string;
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
  text-align: left;
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
    props.transparent ? 'transaparent' : '#f4f5f6'};
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
  min-height: 20px; // Change to the height of your error message
`;
const SelectedValue = styled.span`
  backgroundcolor: blue;
`;
const AutoCompleteSelect: React.FC<InputProps & { mandatory?: boolean }> = ({
  label,
  placeholder,
  error,
  width,
  prefixIcon,
  suffixIcon,
  transparent,
  disabled,
  mandatory = false,
  value,
  onSelect,
  optionList,
  defaultLabel,
  ...props
}) => {
  const shouldShowAsterisk = mandatory;
  // console.log("check option list data--->", optionList)
  const [filteredOptions, setFilteredOptions] = useState([]);
  const [allOptions, setAllOptions] = useState(optionList); // Replace with actual data source
  const [open, setOpen] = useState(false);
  const [values, setValues] = useState('');

  const handleChange = (e) => {
    setValues(e.target.value);
    const filtered = allOptions.filter((option) =>
      option.label.toLowerCase().includes(e.target.value.toLowerCase())
    );
    setFilteredOptions(filtered);
  };

  useEffect(() => {
    setAllOptions(optionList);
    setFilteredOptions(optionList);
    let num: number = value;
    if (num > 0) {
      const matchingObjects = allOptions.filter(
        (obj) => Number(obj.value) === Number(value)
      );
      if (matchingObjects.length > 0) {
        setValues(matchingObjects[0].label);
      } else {
        setValues('');
      }
    } else {
      setValues('');
    }
  }, [value, allOptions, optionList]);

  const inputRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    const handleOutsideClick = (event: MouseEvent) => {
      if (
        inputRef.current &&
        !inputRef.current.contains(event.target as Node)
      ) {
        setOpen(false);
      }
    };
    document.addEventListener('click', handleOutsideClick);
    return () => {
      document.removeEventListener('click', handleOutsideClick);
    };
  }, []);
  const handleClear = () => {
    setValues('');
    onSelect('');
  };

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
          value={values}
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
              justifyContent: 'space-between',
              gap: '10px',
            }}
          >
            {!disabled && values != '' ? (
              <CloseIcon width={10} onClick={(e) => handleClear(e)} />
            ) : (
              ''
            )}

            <DropdownIcon
              onClick={(e) => {
                e.stopPropagation();
                setOpen(!open);
              }}
            />
          </div>
        </SuffixIconWrapper>
      </InputContainer>
      <OptionContainer>
        {open && (
          <OptionList>
            {defaultLabel != null && <li value="">{defaultLabel}</li>}
            {filteredOptions?.map((option) => {
              return (
                <>
                  <li
                    key={option.value}
                    onClick={() => {
                      onSelect(option.value);
                      setOpen(false);
                      setValues(option.label);
                    }}
                    style={{
                      backgroundColor: `${option.label === values ? '#EFF5F5' : ''
                        }`,
                    }}
                  >
                    {option.label}
                  </li>
                </>
              );
            })}
          </OptionList>
        )}
      </OptionContainer>
      {props.errorFree ? (
        <></>
      ) : (
        <ErrorMessageWrapper>
          {error && <InputError>{error}</InputError>}
        </ErrorMessageWrapper>
      )}
    </InputWrapper>
  );
};

export default AutoCompleteSelect;
