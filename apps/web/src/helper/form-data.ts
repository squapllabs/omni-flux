export const objToFormData = (obj: Record<string, any>, formData: FormData): FormData => {
    for (const key in obj) {
      switch (typeof obj[key]) {
        case "boolean":
          formData.append(key, JSON.stringify(obj[key]));
          break;
        case "object":
          if (obj[key]) {
            if (obj[key] instanceof File) {
              formData.append(key, obj[key]);
            } else if (obj[key] instanceof FileList) {
              const files = obj[key] as FileList;
              for (let i = 0; i < files.length; i++) {
                formData.append(key, files[i]);
              }
            } else {
              formData.append(key, JSON.stringify(obj[key]));
            }
          }
          break;
        case "string":
        case "number":
          formData.append(key, obj[key].toString());
          break;
        default:
          break;
      }
    }
  
    return formData;
  };
  
  export const objToFormDataSet = (obj: Record<string, any>, formData: FormData): FormData => {
    for (const key in obj) {
      switch (typeof obj[key]) {
        case "boolean":
          formData.set(key, JSON.stringify(obj[key]));
          break;
        case "object":
          if (obj[key]) {
            formData.set(key, JSON.stringify(obj[key]));
          }
          break;
        case "string":
        case "number":
          formData.set(key, obj[key].toString());
          break;
        default:
          break;
      }
    }
  
    return formData;
  };