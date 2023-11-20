import Styles from '../../styles/ecoProduction.module.scss';
import Button from '../ui/Button';
import { useNavigate } from 'react-router-dom';
import React, { useRef } from 'react';

const EcoProduction = () => {
  const navigate = useNavigate();
  const section1Ref = useRef<HTMLDivElement>(null);
  const section2Ref = useRef<HTMLDivElement>(null);
  const section3Ref = useRef<HTMLDivElement>(null);
  const section4Ref = useRef<HTMLDivElement>(null);
  const section5Ref = useRef<HTMLDivElement>(null);

  const scrollToSection = (ref: any) => {
    if (ref.current) {
      ref.current.scrollIntoView({ behavior: 'smooth' });
    }
  };

  return (
    <div>
      <nav className={Styles.navbar}>
        <div className={Styles.logo}>
          <img src="/Eco Protection.png" alt="aa" width="100%" height="50px" />
        </div>
        {/* <div><a onClick={() => scrollToSection(section1Ref)}>Overview</a></div>
        <div><a onClick={() => scrollToSection(section2Ref)}>Mission</a></div>
        <div><a onClick={() => scrollToSection(section3Ref)}>Vision</a></div> */}
        {/* <div><a onClick={() => scrollToSection(section4Ref)}>Projects</a></div> */}
        {/* <div><a onClick={() => scrollToSection(section5Ref)}>Contact</a></div> */}
        <div className={Styles.loginButton}>
          <Button
            shape="rectangle"
            justify="center"
            size="medium"
            color="outlined"
            onClick={() => navigate('/')}
            className={Styles.btn}
          >
            Login
          </Button>
        </div>
      </nav>
      <div className={Styles.gradient}>
        <h1 className={Styles.heading} ref={section1Ref}>
          About Eco Protection
        </h1>
      </div>
      <div className={Styles.container}>
        <div>
          <img src="founder.jpeg" alt ="founder" className={Styles.images} />
        </div>
        <div className={Styles.overview}>
          <h2 className={Styles.heading2}>Overview</h2>
          <p className={Styles.paragraph}>
            By executing entire spectrum of the project within the roof of the
            company, from concept to commissioning, we could complete the
            projects within the scheduled time and budgeted cost. Our strong
            customer focused approach and continuous quest for quality helps us
            to strongly position ourselves in the business. <br />
            <br />
            We execute the entire infrastructure for water and wastewater and
            also remain focused on our core treatment plant business. Hence, we
            are delivering projects to the current needs of the market, like
            Municipal sewage recycling, Zero liquid discharge plants etc., We
            commit to put our best efforts for implementing the state of art
            technology and being part of circular economy.
          </p>
        </div>
      </div>
      <div className={Styles.container} ref={section2Ref}>
        <div className={Styles.mission}>
          <h2 className={Styles.heading2}>Mission</h2>
          <p className={Styles.paragraph}>
            Our mission is to provide the best quality products to our customers
            and create an impact in the market. Our mission is to provide the
            best quality products to our customers and create an impact in the
            market. Our mission is to provide the best quality products to our
            customers and create an impact in the market.
          </p>
        </div>
        <div className={Styles.vision} ref={section3Ref}>
          <h2 className={Styles.heading2}>Vision</h2>
          <p className={Styles.paragraph}>
            Our vision is to be recognized as the leading brand in the industry
            and to expand our services globally. Our vision is to be recognized
            as the leading brand in the industry and to expand our services
            globally. Our vision is to be recognized as the leading brand in the
            industry and to expand our services globally. Our vision is to be
            recognized as the leading brand in the industry and to expand our
            services globally. Our vision is to be recognized as the leading
            brand in the industry and to expand our services globally.
          </p>
        </div>
      </div>
      {/* <div className={Styles.gradient} ref={section4Ref}>
                <h1 className={Styles.heading}>Self Reliance In EPC Project </h1>
            </div> */}
      {/* <div className={Styles.projectContainer}>
                <div className={Styles.project}>
                    <div>
                        <img src="img1.jpg" alt="wm" className={Styles.projectImages}/>
                        <div className={Styles.overlay}>
                            <div className={Styles.text}>Waste Water Management</div>
                        </div>
                    </div>
                </div>
                <div className={Styles.project}>
                    <img src="supply chain management.jpeg" alt="wm" className={Styles.projectImages}/>
                    <div className={Styles.overlay}>
                        <div className={Styles.text}>Supply Chain Management </div>
                    </div>
                </div>
                <div className={Styles.project}>
                    <img src="civil.jpg" alt="wm" className={Styles.projectImages}/>
                    <div className={Styles.overlay}>
                        <div className={Styles.text}>Civil Work Execution</div>
                    </div>
                </div>  
            </div> */}
      {/* <div className={Styles.projectContainer}>
                <div className={Styles.project}>
                    <div>
                        <img src="Interdisciplinary-Engineering.jpg" alt="wm" className={Styles.projectImages}/>
                        <div className={Styles.overlay}>
                            <div className={Styles.text}>Detail Engineering Of Multidisciplinary Field</div>
                        </div>
                    </div>
                </div>
                <div className={Styles.project}>
                    <img src="Commissioning-Hero.webp" alt="wm" className={Styles.projectImages}/>
                    <div className={Styles.overlay}>
                        <div className={Styles.text}>Plant Commissioning Including Pre-Commissioning, Testing </div>
                    </div>
                </div>
                <div className={Styles.project}>
                    <img src="maintenance masters.jpg" alt="wm" className={Styles.projectImages}/>
                    <div className={Styles.overlay}>
                        <div className={Styles.text}>Operation & Maintenance</div>
                    </div>
                </div>  
            </div> */}
      {/* <div  className={Styles.footer} ref={section5Ref}>
                <div>
                    <h2 className={Styles.heading2}>Our Services</h2>
                    <ul>
                        <li>Industrial Water Treatment Plants</li>
                        <li>Under Ground Drainage Systems</li>
                        <li>Waste water Treatment & Recycle Plants</li>
                        <li>Water Supply & Distribution Projects</li>
                        <li>Potable Water Treatment Plants</li>
                    </ul>
                </div>
                <div>
                    <h2 className={Styles.heading2}>Contact</h2>
                    <ul>
                        <li>Eco Protection Engineers</li>
                        <li>Plot No, 943, 54th St,</li>
                        <li>TVS Colony</li>
                        <li>Anna Nagar West Extension,</li>
                        <li>Chennai, Tamil Nadu 600101</li>
                    </ul>
                </div>
                <div>
                    <h2 className={Styles.heading2}>© 2020 EPEPL</h2>
                </div>
            </div> */}
    </div>
  );
};

export default EcoProduction;
