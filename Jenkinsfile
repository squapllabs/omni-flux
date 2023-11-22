pipeline {
    agent any
    stages {
        stage('checkout code'){
            steps {
                checkout([$class: 'GitSCM',
                branches: [[name: '*/code-analysis-sonar']],
                extensions: scm.extensions,
                userRemoteConfigs: [[
                    url: 'git@github.com:squapllabs/omni-flux.git',
                    credentialsId: 'GitCredentials'
                ]]
            ])
            echo 'git checkout completed'
		          }
            stage('sonarqube analysis'){
                 steps{
                 nodejs(nodeJSInstallation: 'nodeJS'){
                      sh "npm install"
                   }
                 }
               }
            }
 	        }
}
