const generateOTP = () => {
  const min = 100000;
  const max = 999999;
  const otpSecret = Math.floor(Math.random() * (max - min + 1)) + min;
  const otpGeneratedDate = new Date();
  const otpExpirationDate = new Date(
    otpGeneratedDate.getTime() + 10 * 60 * 1000
  ); /* Add 10 minutes validity period */
  const otpResult = {
    otpSecret: otpSecret,
    otpGeneratedDate: otpGeneratedDate,
    otpExpirationDate: otpExpirationDate,
  };
  return otpResult;
};

export default { generateOTP };
