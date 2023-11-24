pipeline {
    agent any
    stages {
        stage('checkout code'){
            steps {
                checkout([$class: 'GitSCM',
                branches: [[name: '*/feature/components-enhancement']],
                extensions: scm.extensions,
                userRemoteConfigs: [[
                    url: 'git@github.com:squapllabs/omni-flux.git',
                    credentialsId: 'GitCredentials'
                ]]
            ])
            echo 'git checkout completed'
		          }
           }
            stage('sonarqube analysis'){
                 steps{
                 nodejs(nodeJSInstallationName: 'NodeJS'){
                      sh "npm install"
                      withSonarQubeEnv('SonarQube_Server'){
                      sh "npm install sonar-scanner"
                      sh "npm run sonar"
                        
                    }
                }
            }
        }
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
        

