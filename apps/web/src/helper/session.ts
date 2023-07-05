import Cookies from 'js-cookie';

export const setCookie = (name: string, value: string, days: number) => {
  Cookies.set(name, value, { expires: days });
};

export const getCookie = (cookieName: string) => {
  const cookies = document.cookie;
  const cookieArray = cookies.split(';');

  for (let i = 0; i < cookieArray.length; i++) {
    const cookie = cookieArray[i].trim();

    if (cookie.indexOf(cookieName + '=') === 0) {
      const token = cookie.substring(cookieName.length + 1);
      return token;
    }
  }
  return null;
};
