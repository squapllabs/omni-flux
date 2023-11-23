import axios from 'axios';

const login = async (req, res) => {
  try {
    const body = req.body;
    const response = await axios.post(
      `${process.env.BACKEND_API_URL}/api/auth/login`,
      body
    );
    if (!response?.data?.status) {
      return res
        .status(response.status)
        .send({ message: 'Invalid email or password' });
    } else {
      return res.status(response.status).send(response.data);
    }
  } catch (e) {
    console.log(e);
    return res.status(500).send('Unable to fetch user details');
  }
};

export { login };
