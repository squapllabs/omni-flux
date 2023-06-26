export const errorMessages = {
    INVALID_FILE_FORMAT: "Please upload a valid image file (JPG or PNG)",
    INVALID_FILE_SIZE: "Please upload an image with a size less than 5 MB",
  };
  
  export const validateImage = (file: File | null): { isValid: boolean; message?: string } => {
    if (
      file &&
      !(
        file.type === "image/jpg" ||
        file.type === "image/jpeg" ||
        file.type === "image/png"
      )
    ) {
      return {
        isValid: false,
        message: errorMessages.INVALID_FILE_FORMAT,
      };
    }
    if (file && file.size > 5000000) {
      return {
        isValid: false,
        message: errorMessages.INVALID_FILE_SIZE,
      };
    }
    return { isValid: true };
  };
  
  export const convertToPascalCase = (string: string | undefined): string => {
    const words = string?.trim().split(" ");
    const capitalizedWords = words?.map((word) => {
      return word.charAt(0).toUpperCase() + word.substring(1).toLowerCase();
    });
    return capitalizedWords?.join(" ") || "";
  };