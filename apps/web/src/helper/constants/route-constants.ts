interface Routes {
    login: string;
    register: string;
    reset: string;
    activate: string;
    welcome: string;
    home: string;
    editPage: (pageId: string) => string;
  }
  
  const routes: Routes = {
    login: "/",
    register: "/register",
    reset: "/reset",
    activate: "/activate",
    welcome: "/welcome",
    home: "/user",
    editPage: (pageId: string) => {
      return `/edit-page/${pageId}`;
    }
  };
  
  export default routes;