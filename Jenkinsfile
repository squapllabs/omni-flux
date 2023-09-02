pipeline {
    agent any
    stages {
        stage('checkout code'){
            steps {
                checkout([$class: 'GitSCM',
                branches: [[name: '*/main']],
                extensions: scm.extensions,
                userRemoteConfigs: [[
                    url: 'git@github.com:squapllabs/omni-flux.git',
                    credentialsId: 'GitCredentials'
                ]]
            ])
            echo 'git checkout completed'
            }
		}
      
      stage('File copy and remove') {
        steps {
            sh 'zip -r /var/lib/jenkins/workspace/Omni_test1.zip /var/lib/jenkins/workspace/Omni_test1'
           // sh 'scp /var/lib/jenkins/workspace/Omni_test1.zip root@192.168.2.27:/root/omniflux_application'
           // sh 'sh /var/lib/jenkins/workspace/omniflux_deploy.sh'
            sh 'sh /var/lib/jenkins/workspace/omniflux_clean_script.sh'

        }
      }
    //     stage('docker image build') {
    //         steps {
    //             echo 'running docker container details'
    //             sh ' docker ps'
				// sh 'whoami'
    //             echo 'docker build started'
    //             sh 'docker build -t omni_flux_image .'
    //             echo 'docker build completed'
    //         }
    //     }
    }     
}

