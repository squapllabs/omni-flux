import React, { ChangeEvent, useState } from 'react';
import Styles from '../../styles/searchBar.module.scss';
import SearchIcon from './icons/search';

interface SearchBarProps {
  onSearch: (searchTerm: string) => void;
}

const SearchBar: React.FC<SearchBarProps> = ({ onSearch }) => {
  const [isExpanded, setIsExpanded] = useState(false);

  const handleSearch = (event: ChangeEvent<HTMLInputElement>) => {
    const searchTerm = event.target.value;
    onSearch(searchTerm);
  };

  const handleFocus = () => {
    setIsExpanded(true);
  };

  const handleBlur = () => {
    setIsExpanded(false);
  };

  return (
    <div>
      <div
        className={`${Styles.searchInputContainer} ${
          isExpanded ? Styles.expanded : ''
        }`}
      >
        <input
          type="text"
          placeholder="Search"
          onChange={handleSearch}
          onFocus={handleFocus}
          onBlur={handleBlur}
          className={Styles.searchInput}
        />
        <div className={Styles.searchIconContainer}>
          <SearchIcon className={Styles.searchIcon} />
        </div>
      </div>
    </div>
  );
};

export default SearchBar;
