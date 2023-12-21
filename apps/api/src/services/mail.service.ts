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
                        ${process.env.COMPANY_NAME}
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

/**
 * Method to Send Email To vendor for the Purchase Request
 */
const purchaseRequestEmailForVendor = async (body) => {
  try {
    const fromMailId = process.env.EMAIL_FROM;
    const purchase_request_details = body.purchase_request_details;
    const vendor_name = body.vendor_name;
    const to_email_id = body.to_email_id;
    const currentDate = new Date();
    const currentYear = currentDate.getFullYear();

    const template = `<!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>Purchase Request Notification</title>
      </head>
      <body
        style="
          font-family: Arial, sans-serif;
          background-color: #f4f4f4;
          margin: 0;
          padding: 0;
        "
      >
        <table width="100%" cellspacing="0" cellpadding="0">
          <tr>
            <td align="center">
              <table
                class="container"
                width="600"
                cellpadding="20"
                style="
                  background-color: #ffffff;
                  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                  border-radius: 10px;
                  padding: 20px;
                "
              >
                <tr>
                  <td
                    align="center"
                    bgcolor="#e9e5e5"
                    style="
                      color: #ffffff;
                      border-top-left-radius: 10px;
                      border-top-right-radius: 10px;
                    "
                  >
                    <h1 style="color: #333; font-size: 24px; margin-bottom: 20px">
                      Purchase Request Notification
                    </h1>
                  </td>
                </tr>
                <tr>
                  <td>
                    <p>Dear <b>${vendor_name},</b></p>
                    <p>
                      We are pleased to inform you that a new purchase request has
                      been created for the following items:
                    </p>
                    <table
                      style="
                        width: 100%;
                        border-collapse: collapse;
                        margin-top: 5px;
                      "
                    >
                      <tr>
                        <th
                          style="
                            border: 1px solid #dddddd;
                            text-align: left;
                            padding: 3px;
                            background-color: #f2f2f2;
                          "
                        >
                          Item Name
                        </th>
                        <th
                          style="
                            border: 1px solid #dddddd;
                            text-align: left;
                            padding: 3px;
                            background-color: #f2f2f2;
                          "
                        >
                          Quantity
                        </th>
                      </tr>
    
                      ${purchase_request_details
                        .map(
                          (item) => `
                      <tr>
                        <td style="border: 1px solid #dddddd; padding: 3px">
                          ${item.item_name}
                        </td>
                        <td style="border: 1px solid #dddddd; padding: 3px">
                          ${item.purchase_requested_quantity}
                        </td>
                      </tr>
                      `
                        )
                        .join('')}
                    </table>
    
                    <p>
                      We kindly request you to review this purchase request and
                      provide us with your quotation to fulfill these requirements.
                      Your prompt response would be greatly appreciated as it will
                      help us expedite the procurement process.
                    </p>
                    <p>Thank you for your prompt attention to this matter.</p>
                    <div style="margin-top: 20px">
                      <p>Sincerely,</p>
                      <p><b>The Omni-Flux ERP</b></p>
                    </div>
                    <div
                      class="footer"
                      style="
                        background-color: #e9e5e5;
                        color: #333;
                        padding: 15px;
                        text-align: center;
                        border-bottom-left-radius: 10px;
                        border-bottom-right-radius: 10px;
                        font-size: 14px;
                      "
                    >
                      <p>
                        <b
                          >&copy; ${currentYear} ${
      process.env.COMPANY_NAME
    }. All rights
                          reserved.</b
                        >
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
      subject:
        'Request for Quotation - New Purchase Request from Omni-Flux ERP',
      html: template,
    });

    console.log('email sent sucessfully');
  } catch (error) {
    console.log(error, 'email not sent');
  }
};

/**
 * Method to Send Email for Forgot Password
 */
const forgotPasswordEmail = async (email_id: string, link: string) => {
  try {
    const fromMailId = process.env.EMAIL_FROM;
    const resetPasswordLink = link;
    const to_email_id = email_id;

    const template = `<!DOCTYPE html>
    <html>
      <head>
        <title>Forgot Password</title>
      </head>
    
      <body
        style="
          font-family: 'Arial', sans-serif;
          margin: 0;
          padding: 0;
          background-color: #f5f5f5;
        "
      >
        <table
          role="presentation"
          style="width: 100%; border-collapse: collapse; border: none"
        >
          <tr>
            <td align="center" style="padding: 2rem">
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
                          font-size: 28px;
                          font-weight: bold;
                          color: #3498db;
                        "
                      >
                        Forgot Your Password?
                      </h1>
                      <p
                        style="
                          padding: 16px 0 24px;
                          font-size: 18px;
                          color: #555555;
                        "
                      >
                        We received a request to reset your password. Click the
                        button below to reset it:
                      </p>
                      <a
                        style="
                          display: inline-block;
                          background-color: #3498db;
                          color: #ffffff;
                          font-size: 18px;
                          padding: 12px 24px;
                          text-decoration: none;
                          border-radius: 8px;
                          cursor: pointer;
                        "
                        href=${resetPasswordLink}
                      >
                        Reset Password
                      </a>
                      <p style="padding: 24px 0; font-size: 18px; color: #555555">
                        If you did not request a password reset, please disregard
                        this email.
                      </p>
                      <p
                        style="
                          padding: 0;
                          margin: 0;
                          font-size: 16px;
                          color: #888888;
                        "
                      >
                        Best Regards,<br />
                        ${process.env.COMPANY_NAME}
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
      subject: 'Secure Your Account: Reset Password for Omni-Flux ERP',
      html: template,
    });

    console.log('email sent sucessfully');
  } catch (error) {
    console.log(error, 'email not sent');
  }
};

export default {
  OTPEmail,
  purchaseRequestEmailForVendor,
  forgotPasswordEmail,
};
