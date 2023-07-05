/* Local Storae Authorization */

export const setItem = (key: string, value: string) => {
  try {
    const serializedValue = JSON.stringify(value);
    localStorage.setItem(key, serializedValue);
  } catch (error) {
    console.error('Error setting item in app local storage:', error);
  }
};

export const getItem = (key: string) => {
  try {
    const serializedValue = localStorage.getItem(key);
    return serializedValue ? JSON.parse(serializedValue) : null;
  } catch (error) {
    console.error('Error getting item from app local storage:', error);
    return null;
  }
};
