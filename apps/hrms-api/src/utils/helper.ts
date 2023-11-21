import cookie from 'cookie';

const validateCookie = (req: any, res: any, next) => {
  const { cookies } = req;
  console.log('check cookies data-->', cookies);
  // if ('session_id' in cookies) {
  //     console.log('Session ID Exists');
  // }
  next();
};

function setCookie(res, name, value, isRememberMe = true) {
  return new Promise((resolve) => {
    const serializedCookie = cookie.serialize(name, value, {
      httpOnly: false,
      maxAge: isRememberMe ? 365 * 24 * 60 * 60 : null,
      sameSite: process.env.NODE_ENV === 'LOCAL' ? 'strict' : 'none',
      secure: process.env.NODE_ENV === 'LOCAL' ? false : true,
      domain:
        process.env.NODE_ENV === 'LOCAL'
          ? 'localhost'
          : process.env.COOKIE_DOMAIN,
    });

    const previousCookies = res.getHeader('Set-Cookie') || [];
    const newCookie = Array.isArray(previousCookies)
      ? [...previousCookies, serializedCookie]
      : [previousCookies, serializedCookie];

    res.setHeader('Set-Cookie', newCookie);

    resolve('');
  });
}

export { setCookie, validateCookie };
