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
            sh 'sh /root/omniflux_dep/omniflux_deploy.sh'
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

