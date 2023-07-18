/* eslint-disable @typescript-eslint/no-explicit-any */

import Layout from '../layout/layout';
import ProtectedRoute from '../routes/ProtectedRoute';

// This is a higher-order component that wraps the provided Component in
// a Layout and ProtectedRoute
function withLayoutAndProtection(Component: any) {
  return function WrappedComponent(props: any) {
    return (
      <ProtectedRoute>
        <Layout>
          <Component {...props} />
        </Layout>
      </ProtectedRoute>
    );
  }
}

export default withLayoutAndProtection;
