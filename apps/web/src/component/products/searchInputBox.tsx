import React, { ChangeEvent } from 'react';

interface SearchInputProps {
  placeholder?: string;
  onSearch: (searchTerm: string) => void;
}

const SearchInput: React.FC<SearchInputProps> = ({
  placeholder = 'Search',
  onSearch,
}) => {
  const handleChange = (event: ChangeEvent<HTMLInputElement>) => {
    const searchTerm = event.target.value;
    onSearch(searchTerm);
  };

  return (
    <input type="text" placeholder={placeholder} onChange={handleChange} />
  );
};

export default SearchInput;
