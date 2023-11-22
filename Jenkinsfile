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
            withSonarQubeEnv(installationName: 'SonarQube_Server') {
	        sh 'mvn clean package sonar:sonar'
		          }
            }
 	        }
      }     
}
