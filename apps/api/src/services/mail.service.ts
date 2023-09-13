import transporter from '../utils/emailConfiguration';

/**
 * Method to Send Email for OTP Verification
 */
const OTPEmail = async (body) => {
  const fromMailId = process.env.EMAIL_FROM;
  const otp = body.otp_secret;
  const user_name = body.user_name;
  const to_email_id = body.to_email_id;

  try {
    const template = `<!DOCTYPE html>
    <html>
      <head>
        <title>Verify Your Login</title>
      </head>
      <body style="font-family: Arial, sans-serif; margin: 0; padding: 0">
        <table
          role="presentation"
          style="
            width: 100%;
            border-collapse: collapse;
            border: none;
            background-color: #f5f5f5;
          "
        >
          <tr>
            <td align="center" style="padding: 1rem 2rem">
              <table
                role="presentation"
                style="
                  max-width: 600px;
                  width: 100%;
                  border-collapse: collapse;
                  border: none;
                  text-align: center;
                  background-color: #ffffff;
                  border-radius: 8px;
                  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                "
              >
                <tr>
                  <td style="padding: 40px 0">
                    <div style="text-align: center">
                      <h1
                        style="
                          margin: 0;
                          font-size: 24px;
                          font-weight: bold;
                          color: #007bff;
                        "
                      >
                        Account Verification Code
                      </h1>
                      <p style="padding: 16px 0 24px; font-size: 18px">
                        Dear <b>${user_name},</b><br />
                        As part of our security measures, we require you to enter
                        the verification code below to access your account. Please
                        use this code within the next 10 minutes.
                      </p>
                      <table
                        role="presentation"
                        style="
                          border-collapse: collapse;
                          border: none;
                          font-size: 28px;
                          font-weight: bold;
                          background-color: #007bff;
                          color: #ffffff;
                          border-radius: 8px;
                          padding: 16px 32px;
                          display: inline-block;
                        "
                      >
                        <tr>
                          <td>${otp}</td>
                        </tr>
                      </table>
                      <p style="padding: 24px 0; font-size: 18px">
                        If you did not request this verification code, please
                        disregard this email.
                      </p>
                      <p style="padding: 0; margin: 0; font-size: 16px">
                        Best Regards,<br />
                        The OmniFlux ERP Team
                      </p>
                    </div>
                  </td>
                </tr>
              </table>
            </td>
          </tr>
        </table>
      </body>
    </html>`;

    await transporter.transporter.sendMail({
      from: `${fromMailId}`,
      to: to_email_id,
      subject: 'Two Factor Authentication',
      html: template,
    });

    console.log('email sent sucessfully');
  } catch (error) {
    console.log(error, 'email not sent');
  }
};

export default {
  OTPEmail,
};
