import React, { useState, ChangeEvent, FormEvent } from 'react';

interface SearchInputProps {
  placeholder?: string;
  onSearch: (searchTerm: string) => void;
}

const SearchInput: React.FC<SearchInputProps> = ({ placeholder, onSearch }) => {
  const [searchTerm, setSearchTerm] = useState('');

  const handleChange = (event: ChangeEvent<HTMLInputElement>) => {
    setSearchTerm(event.target.value);
  };

  const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    onSearch(searchTerm);
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="text"
        placeholder={placeholder}
        value={searchTerm}
        onChange={handleChange}
        style={{ outline: 'none' }}
      />
    </form>
  );
};

export default SearchInput;
