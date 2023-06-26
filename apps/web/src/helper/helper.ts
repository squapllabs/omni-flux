const removeEndComma = (str: string): string => {
    return str.replace(/,\s*$/, "");
  };
  
  const removeStartEndComma = (str: string): string => {
    return str.replace(/^\s*,+\s*|\s*,+\s*$/g, "");
  };
  
  const getJSDateObject = (inputDate: string): string => {
    const date = new Date(inputDate);
    const offset = date.getTimezoneOffset();
    if (offset < 0) {
      date.setHours(12, 0, 0);
    }
    return date.toISOString().substring(0, 10);
  };
  
  export { getJSDateObject, removeStartEndComma, removeEndComma };